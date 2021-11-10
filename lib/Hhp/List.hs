module Hhp.List (listModules, modules) where

import GHC (Ghc)
import qualified GHC as G

import GHC.Driver.Session (DynFlags(..))
import GHC.Unit.Module.Name (moduleNameString)
import GHC.Unit.State (lookupModuleInAllUnits, listVisibleModuleNames)
import GHC.Unit.Types (moduleName)

import Control.Monad.Catch (SomeException(..), catch)
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
modules opt = convert opt . arrange <$> (getModules `catch` handler)
  where
    getModules = listVisibleModules <$> G.getSessionDynFlags
    arrange = nub . sort . map (moduleNameString . moduleName)
    handler (SomeException _) = return []

----------------------------------------------------------------

listVisibleModules :: DynFlags -> [G.Module]
listVisibleModules df = mods
  where
    state = unitState df
    modNames = listVisibleModuleNames state
    mods = [ m | mn <- modNames, (m, _) <- lookupModuleInAllUnits state mn ]
