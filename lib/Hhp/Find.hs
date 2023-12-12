{-# LANGUAGE BangPatterns #-}

module Hhp.Find (
    Symbol,
    SymMdlDb,
    findSymbol,
    getSymMdlDb,
    lookupSym,
) where

import GHC (DynFlags, Ghc, Module, ModuleInfo)
import qualified GHC as G
import GHC.Driver.Session (initSDocContext)
import GHC.Unit.Info (UnitInfo, mkUnit, unitExposedModules)
import GHC.Unit.State (UnitState, listUnitInfo)
import GHC.Utils.Outputable (ppr)

import Control.DeepSeq (force)
import Control.Monad.Catch (SomeException (..), catch)
import Data.Function (on)
import Data.List (groupBy, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)

import Hhp.Doc (showOneLine, styleUnqualified)
import Hhp.GHCApi
import Hhp.Gap
import Hhp.Types

-- | Type of key for `SymMdlDb`.
type Symbol = String

-- | Database from 'Symbol' to modules.
newtype SymMdlDb = SymMdlDb (Map Symbol [ModuleString])

-- | Finding modules to which the symbol belong.
findSymbol :: Options -> Cradle -> Symbol -> IO String
findSymbol opt cradle sym = withGHC' $ do
    initializeFlagsWithCradle opt cradle
    lookupSym opt sym <$> getSymMdlDb

-- | Creating 'SymMdlDb'.
getSymMdlDb :: Ghc SymMdlDb
getSymMdlDb = do
    sm <- G.getSessionDynFlags >>= browseAll
    let !sms = force $ map tieup $ groupBy ((==) `on` fst) $ sort sm
        !m = force $ M.fromList sms
    return (SymMdlDb m)
  where
    tieup x = (head (map fst x), map snd x)

-- | Looking up 'SymMdlDb' with 'Symbol' to find modules.
lookupSym :: Options -> Symbol -> SymMdlDb -> String
lookupSym opt sym (SymMdlDb db) = convert opt $ fromMaybe [] (M.lookup sym db)

----------------------------------------------------------------

-- | Browsing all functions in all system/user modules.
browseAll :: DynFlags -> Ghc [(String, String)]
browseAll dflag = do
    ms <- packageModules <$> getUnitState
    is <- catMaybes <$> mapM getMaybeModuleInfo ms
    return $ concatMap (toNameModule dflag) (zip ms is)

-- ghc-bignum causes errors, sigh.
getMaybeModuleInfo :: Module -> Ghc (Maybe (Maybe ModuleInfo))
getMaybeModuleInfo x = Just <$> G.getModuleInfo x `catch` (\(SomeException _) -> return Nothing)

toNameModule :: DynFlags -> (Module, Maybe ModuleInfo) -> [(String, String)]
toNameModule _ (_, Nothing) = []
toNameModule dflag (m, Just inf) = map (\name -> (toStr name, mdl)) names
  where
    mdl = G.moduleNameString (G.moduleName m)
    names = G.modInfoExports inf
    toStr = showOneLine (initSDocContext dflag styleUnqualified) . ppr

packageModules :: UnitState -> [Module]
packageModules us = concatMap fromUnitInfo $ listUnitInfo us

fromUnitInfo :: UnitInfo -> [Module]
fromUnitInfo uinfo = modules
  where
    uid = mkUnit uinfo
    moduleNames = map fst $ unitExposedModules uinfo
    modules = map (G.mkModule uid) moduleNames
