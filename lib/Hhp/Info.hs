{-# LANGUAGE TupleSections, FlexibleInstances, Rank2Types #-}

module Hhp.Info (
    infoExpr
  , info
  , typeExpr
  , types
  ) where

import CoreMonad (liftIO)
import CoreUtils (exprType)
import Desugar (deSugarExpr)
import Exception (ghandle, SomeException(..))
import GHC (Ghc, TypecheckedModule(..), DynFlags, SrcSpan, Type, GenLocated(L))
import qualified GHC as G

import HscTypes (ModSummary)
import Outputable (PprStyle)
import PprTyThing
import TcHsSyn (hsPatType)

import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord as O

import Hhp.Doc (showPage, showOneLine, getStyle)
import Hhp.GHCApi
import Hhp.Gap
import Hhp.Logger (getSrcSpan)
import Hhp.Syb
import Hhp.Things
import Hhp.Types

----------------------------------------------------------------

-- | Obtaining information of a target expression. (GHCi's info:)
infoExpr :: Options
         -> Cradle
         -> FilePath     -- ^ A target file.
         -> Expression   -- ^ A Haskell expression.
         -> IO String
infoExpr opt cradle file expr = withGHC' $ do
    initializeFlagsWithCradle opt cradle
    info opt file expr

-- | Obtaining information of a target expression. (GHCi's info:)
info :: Options
     -> FilePath     -- ^ A target file.
     -> Expression   -- ^ A Haskell expression.
     -> Ghc String
info opt file expr = convert opt <$> ghandle handler body
  where
    body = inModuleContext file $ \dflag style -> do
        sdoc <- infoThing expr
        return $ showPage dflag style sdoc
    handler (SomeException _) = return "Cannot show info"

----------------------------------------------------------------

-- | Obtaining type of a target expression. (GHCi's type:)
typeExpr :: Options
         -> Cradle
         -> FilePath     -- ^ A target file.
         -> Int          -- ^ Line number.
         -> Int          -- ^ Column number.
         -> IO String
typeExpr opt cradle file lineNo colNo = withGHC' $ do
    initializeFlagsWithCradle opt cradle
    types opt file lineNo colNo

-- | Obtaining type of a target expression. (GHCi's type:)
types :: Options
      -> FilePath     -- ^ A target file.
      -> Int          -- ^ Line number.
      -> Int          -- ^ Column number.
      -> Ghc String
types opt file lineNo colNo = convert opt <$> ghandle handler body
  where
    body = inModuleContext file $ \dflag style -> do
        modSum <- fileModSummary file
        srcSpanTypes <- getSrcSpanType modSum lineNo colNo
        return $ map (toTup dflag style) $ sortBy (cmp `on` fst) srcSpanTypes
    handler (SomeException _) = return []

getSrcSpanType :: G.ModSummary -> Int -> Int -> Ghc [(SrcSpan, Type)]
getSrcSpanType modSum lineNo colNo = do
    p <- G.parseModule modSum
    tcm@TypecheckedModule{tm_typechecked_source = tcs} <- G.typecheckModule p
    let es = listifySpans tcs (lineNo, colNo) :: [LExpression]
        bs = listifySpans tcs (lineNo, colNo) :: [LBinding]
        ps = listifySpans tcs (lineNo, colNo) :: [LPattern]
    ets <- mapM (getType tcm) es
    bts <- mapM (getType tcm) bs
    pts <- mapM (getType tcm) ps
    return $ catMaybes $ concat [ets, bts, pts]

cmp :: SrcSpan -> SrcSpan -> Ordering
cmp a b
  | a `G.isSubspanOf` b = O.LT
  | b `G.isSubspanOf` a = O.GT
  | otherwise           = O.EQ

toTup :: DynFlags -> PprStyle -> (SrcSpan, Type) -> ((Int,Int,Int,Int),String)
toTup dflag style (spn, typ) = (fourInts spn, pretty dflag style typ)

fourInts :: SrcSpan -> (Int,Int,Int,Int)
fourInts = fromMaybe (0,0,0,0) . getSrcSpan

pretty :: DynFlags -> PprStyle -> Type -> String
pretty dflag style = showOneLine dflag style . pprTypeForUser

----------------------------------------------------------------

inModuleContext :: FilePath -> (DynFlags -> PprStyle -> Ghc a) -> Ghc a
inModuleContext file action =
    withDynFlags (setWarnTypedHoles . setDeferTypeErrors . setNoWaringFlags) $ do
    setTargetFiles [file]
    withContext $ do
        dflag <- G.getSessionDynFlags
        style <- getStyle dflag
        action dflag style

----------------------------------------------------------------

fileModSummary :: FilePath -> Ghc ModSummary
fileModSummary file = do
    mss <- getModSummaries <$> G.getModuleGraph
    let [ms] = filter (\m -> G.ml_hs_file (G.ms_location m) == Just file) mss
    return ms

withContext :: Ghc a -> Ghc a
withContext action = G.gbracket setup teardown body
  where
    setup = G.getContext
    teardown = setCtx
    body _ = do
        topImports >>= setCtx
        action
    topImports = do
        mss <- getModSummaries <$> G.getModuleGraph
        map modName <$> filterM isTop mss
    isTop mos = lookupMod mos <|> returnFalse
    lookupMod mos = G.lookupModule (G.ms_mod_name mos) Nothing >> return True
    returnFalse = return False
    modName = G.IIModule . G.moduleName . G.ms_mod
    setCtx = G.setContext

----------------------------------------------------------------

class HasType a where
    getType :: TypecheckedModule -> a -> Ghc (Maybe (SrcSpan, Type))

instance HasType LExpression where
    getType _ e = do
        hs_env <- G.getSession
        mbe <- liftIO $ snd <$> deSugarExpr hs_env e
        return $ (G.getLoc e, ) . CoreUtils.exprType <$> mbe

instance HasType LBinding where
    getType _ (L spn FunBind{fun_matches = m}) = return $ Just (spn, typ)
      where
        in_tys  = inTypes m
        out_typ = outType m
        typ = mkFunTys in_tys out_typ
    getType _ _ = return Nothing

instance HasType LPattern where
    getType _ (G.L spn pat) = return $ Just (spn, hsPatType pat)
