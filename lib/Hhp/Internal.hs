-- | Low level access to the Hhp library.

module Hhp.Internal (
  -- * Types
    GHCOption
  , Package
  , PackageBaseName
  , PackageVersion
  , PackageId
  , IncludeDir
  , CompilerOptions(..)
  -- * Cabal API
  , parseCabalFile
  , getCompilerOptions
  , cabalAllBuildInfo
  , cabalDependPackages
  , cabalSourceDirs
  , cabalAllTargets
  -- * IO
  , getSystemLibDir
  , getDynamicFlags
  -- * Initializing 'DynFlags'
  , initializeFlagsWithCradle
  -- * Targets
  , setTargetFiles
  -- * Logging
  , withLogger
  , setNoWaringFlags
  , setAllWaringFlags
  ) where

import Hhp.CabalApi
import Hhp.GHCApi
import Hhp.Logger
import Hhp.Types
