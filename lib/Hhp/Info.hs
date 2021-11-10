{-# LANGUAGE TupleSections, FlexibleInstances, Rank2Types #-}

module Hhp.Info (
    infoExpr
  , info
  , typeExpr
  , types
  ) where

import GHC (Ghc, TypecheckedModule(..), DynFlags, SrcSpan, Type, GenLocated(L), ModSummary, mgModSummaries, mg_ext, LHsBind, LHsExpr, Type, Located, Pat)
import qualified GHC as G
import GHC.Core.Ppr.TyThing
import GHC.Core.Type (mkVisFunTys)
import GHC.Core.Utils (exprType)
import GHC.Hs.Binds (HsBindLR(..))
import GHC.Hs.Expr (MatchGroupTc(..))
import GHC.Hs.Extension (GhcTc)
import GHC.HsToCore (deSugarExpr)
import GHC.Tc.Utils.Zonk (hsPatType)
import GHC.Utils.Monad (liftIO)
import GHC.Utils.Outputable (PprStyle)

import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Control.Monad.Catch (SomeException(..), handle, bracket)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord as O

import Hhp.Doc (showPage, showOneLine, getStyle)
import Hhp.GHCApi
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
info opt file expr = convert opt <$> handle handler body
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
types opt file lineNo colNo = convert opt <$> handle handler body
  where
    body = inModuleContext file $ \dflag style -> do
        modSum <- fileModSummary file
        srcSpanTypes <- getSrcSpanType modSum lineNo colNo
        return $ map (toTup dflag style) $ sortBy (cmp `on` fst) srcSpanTypes
    handler (SomeException _) = return []

type LExpression = LHsExpr GhcTc
type LBinding    = LHsBind GhcTc
type LPattern    = Located (Pat GhcTc)

getSrcSpanType :: ModSummary -> Int -> Int -> Ghc [(SrcSpan, Type)]
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
        style <- getStyle
        action dflag style

----------------------------------------------------------------

fileModSummary :: FilePath -> Ghc ModSummary
fileModSummary file = do
    mss <- mgModSummaries <$> G.getModuleGraph
    let [ms] = filter (\m -> G.ml_hs_file (G.ms_location m) == Just file) mss
    return ms

withContext :: Ghc a -> Ghc a
withContext action = bracket setup teardown body
  where
    setup = G.getContext
    teardown = setCtx
    body _ = do
        topImports >>= setCtx
        action
    topImports = do
        mss <- mgModSummaries <$> G.getModuleGraph
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
        return $ (G.getLoc e, ) . exprType <$> mbe

instance HasType LBinding where
    getType _ (L spn FunBind{fun_matches = m}) = return $ Just (spn, typ)
      where
        in_tys  = mg_arg_tys $ mg_ext m
        out_typ = mg_res_ty  $ mg_ext m
        typ = mkVisFunTys in_tys out_typ
    getType _ _ = return Nothing

instance HasType LPattern where
    getType _ (G.L spn pat) = return $ Just (spn, hsPatType pat)
