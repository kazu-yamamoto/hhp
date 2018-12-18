module Hhp.List (listModules, modules) where

import DynFlags (DynFlags)
import GHC (Ghc)
import qualified GHC as G
import Module (moduleNameString, moduleName)
import Packages (lookupModuleInAllPackages, listVisibleModuleNames)

import Control.Exception (SomeException(..))
import Data.List (nub, sort)

import Hhp.GHCApi
import Hhp.Types

----------------------------------------------------------------

-- | Listing installed modules.
listModules :: Options -> Cradle -> IO String
listModules opt cradle = withGHC' $ do
    initializeFlagsWithCradle opt cradle
    modules opt

-- | Listing installed modules.
modules :: Options -> Ghc String
modules opt = convert opt . arrange <$> (getModules `G.gcatch` handler)
  where
    getModules = listVisibleModules <$> G.getSessionDynFlags
    arrange = nub . sort . map (moduleNameString . moduleName)
    handler (SomeException _) = return []

----------------------------------------------------------------

listVisibleModules :: DynFlags -> [G.Module]
listVisibleModules df = mods
  where
    modNames = listVisibleModuleNames df
    mods = [ m | mn <- modNames, (m, _) <- lookupModuleInAllPackages df mn ]
