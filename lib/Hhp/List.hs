module Hhp.List (listModules, modules) where

import GHC (Ghc)
import qualified GHC as G

import GHC.Unit.State (listVisibleModuleNames, lookupModuleInAllUnits)
import GHC.Unit.Types (moduleName)

import Control.Monad.Catch (SomeException (..), catch)
import Data.List (nub, sort)

import Hhp.GHCApi
import Hhp.Gap
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
    arrange = nub . sort . map (G.moduleNameString . moduleName)
    handler (SomeException _) = return []

----------------------------------------------------------------

getModules :: Ghc [G.Module]
getModules = do
    us <- getUnitState
    let modNames = listVisibleModuleNames us
    return [m | mn <- modNames, (m, _) <- lookupModuleInAllUnits us mn]
