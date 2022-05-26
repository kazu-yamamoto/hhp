-- | The Happy Haskell Programming library in low level.

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
  , getDynamicFlags
  -- * Targets
  , setTargetFiles
  -- * Logging
  , withLogger
  , setNoWarningFlags
  , setAllWarningFlags
  ) where

import Hhp.CabalApi
import Hhp.GHCApi
import Hhp.Logger
import Hhp.Types
