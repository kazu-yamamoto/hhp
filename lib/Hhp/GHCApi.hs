{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module Hhp.GHCApi (
    withGHC
  , withGHC'
  , initializeFlagsWithCradle
  , setTargetFiles
  , getDynamicFlags
  , getSystemLibDir
  , withDynFlags
  , withCmdFlags
  , setNoWaringFlags
  , setAllWaringFlags
  ) where

import CoreMonad (liftIO)
import DynFlags (GeneralFlag(Opt_BuildingCabalPackage, Opt_HideAllPackages), gopt_set, ModRenaming(..), PackageFlag(ExposePackage), PackageArg(..))
import Exception (ghandle, SomeException(..))
import GHC (Ghc, DynFlags(..), GhcLink(..), HscTarget(..), LoadHowMuch(..))
import qualified GHC as G

import Control.Applicative ((<|>))
import Control.Monad (forM, void)
import Data.Maybe (isJust, fromJust)
import System.Exit (exitSuccess)
import System.IO (hPutStr, hPrint, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)

import Hhp.CabalApi
import qualified Hhp.Gap as Gap
import Hhp.GhcPkg
import Hhp.Types

----------------------------------------------------------------

-- | Obtaining the directory for system libraries.
getSystemLibDir :: IO (Maybe FilePath)
getSystemLibDir = do
    res <- readProcess "ghc" ["--print-libdir"] []
    return $ case res of
        ""   -> Nothing
        dirn -> Just (init dirn)

----------------------------------------------------------------

-- | Converting the 'Ghc' monad to the 'IO' monad.
withGHC :: FilePath  -- ^ A target file displayed in an error message.
        -> Ghc a -- ^ 'Ghc' actions created by the Ghc utilities.
        -> IO a
withGHC file body = ghandle ignore $ withGHC' body
  where
    ignore :: SomeException -> IO a
    ignore e = do
        hPutStr stderr $ file ++ ":0:0:Error:"
        hPrint stderr e
        exitSuccess

withGHC' :: Ghc a -> IO a
withGHC' body = do
    mlibdir <- getSystemLibDir
    G.runGhc mlibdir $ do
        dflags <- G.getSessionDynFlags
        G.defaultCleanupHandler dflags body

----------------------------------------------------------------

importDirs :: [IncludeDir]
importDirs = [".","..","../..","../../..","../../../..","../../../../.."]

data Build = CabalPkg | SingleFile deriving Eq

-- | Initialize the 'DynFlags' relating to the compilation of a single
-- file or GHC session according to the 'Cradle' and 'Options'
-- provided.
initializeFlagsWithCradle ::
           Options
        -> Cradle
        -> Ghc ()
initializeFlagsWithCradle opt cradle
  | cabal     = withCabal <|> withSandbox
  | otherwise = withSandbox
  where
    mCradleFile = cradleCabalFile cradle
    cabal = isJust mCradleFile
    ghcopts = ghcOpts opt
    withCabal = do
        pkgDesc <- liftIO $ parseCabalFile $ fromJust mCradleFile
        compOpts <- liftIO $ getCompilerOptions ghcopts cradle pkgDesc
        initSession CabalPkg opt compOpts
    withSandbox = initSession SingleFile opt compOpts
      where
        pkgOpts = ghcDbStackOpts $ cradlePkgDbStack cradle
        compOpts
          | null pkgOpts = CompilerOptions ghcopts importDirs []
          | otherwise    = CompilerOptions (ghcopts ++ pkgOpts) [wdir,rdir] []
        wdir = cradleCurrentDir cradle
        rdir = cradleRootDir    cradle

----------------------------------------------------------------

initSession :: Build
            -> Options
            -> CompilerOptions
            -> Ghc ()
initSession build Options {..} CompilerOptions {..} = do
    df <- G.getSessionDynFlags
    void $ G.setSessionDynFlags =<< (addCmdOpts ghcOptions
      $ setLinkerOptions
      $ setIncludeDirs includeDirs
      $ setBuildEnv build
      $ setEmptyLogger
      $ addPackageFlags depPackages df)

setEmptyLogger :: DynFlags -> DynFlags
setEmptyLogger df = df { G.log_action =  \_ _ _ _ _ _ -> return () }

----------------------------------------------------------------

-- we don't want to generate object code so we compile to bytecode
-- (HscInterpreted) which implies LinkInMemory
-- HscInterpreted
setLinkerOptions :: DynFlags -> DynFlags
setLinkerOptions df = df {
    ghcLink   = LinkInMemory
  , hscTarget = HscInterpreted
  }

setIncludeDirs :: [IncludeDir] -> DynFlags -> DynFlags
setIncludeDirs idirs df = df { importPaths = idirs }

setBuildEnv :: Build -> DynFlags -> DynFlags
setBuildEnv build = setHideAllPackages build . setCabalPackage build

-- At the moment with this option set ghc only prints different error messages,
-- suggesting the user to add a hidden package to the build-depends in his cabal
-- file for example
setCabalPackage :: Build -> DynFlags -> DynFlags
setCabalPackage CabalPkg df = setCabalPkg df
setCabalPackage _ df = df

-- | Enable hiding of all package not explicitly exposed (like Cabal does)
setHideAllPackages :: Build -> DynFlags -> DynFlags
setHideAllPackages CabalPkg df = gopt_set df Opt_HideAllPackages
setHideAllPackages _ df        = df

-- | Parse command line ghc options and add them to the 'DynFlags' passed
addCmdOpts :: [GHCOption] -> DynFlags -> Ghc DynFlags
addCmdOpts cmdOpts df =
    tfst <$> G.parseDynamicFlags df (map G.noLoc cmdOpts)
  where
    tfst (a,_,_) = a

----------------------------------------------------------------

-- | Set the files as targets and load them.
setTargetFiles :: [FilePath] -> Ghc ()
setTargetFiles files = do
    targets <- forM files $ \file -> G.guessTarget file Nothing
    G.setTargets targets
    void $ G.load LoadAllTargets

----------------------------------------------------------------

-- | Return the 'DynFlags' currently in use in the GHC session.
getDynamicFlags :: IO DynFlags
getDynamicFlags = do
    mlibdir <- getSystemLibDir
    G.runGhc mlibdir G.getSessionDynFlags

withDynFlags :: (DynFlags -> DynFlags) -> Ghc a -> Ghc a
withDynFlags setFlag body = G.gbracket setup teardown (\_ -> body)
  where
    setup = do
        dflag <- G.getSessionDynFlags
        void $ G.setSessionDynFlags (setFlag dflag)
        return dflag
    teardown = void . G.setSessionDynFlags

withCmdFlags :: [GHCOption] -> Ghc a -> Ghc a
withCmdFlags flags body = G.gbracket setup teardown (\_ -> body)
  where
    setup = do
        dflag <- G.getSessionDynFlags >>= addCmdOpts flags
        void $ G.setSessionDynFlags dflag
        return dflag
    teardown = void . G.setSessionDynFlags

----------------------------------------------------------------

-- | Set 'DynFlags' equivalent to "-w:".
setNoWaringFlags :: DynFlags -> DynFlags
setNoWaringFlags df = df { warningFlags = Gap.emptyWarnFlags}

-- | Set 'DynFlags' equivalent to "-Wall".
setAllWaringFlags :: DynFlags -> DynFlags
setAllWaringFlags df = df { warningFlags = allWarningFlags }

{-# NOINLINE allWarningFlags #-}
allWarningFlags :: Gap.WarnFlags
allWarningFlags = unsafePerformIO $ do
    mlibdir <- getSystemLibDir
    G.runGhc mlibdir $ do
        df <- G.getSessionDynFlags
        df' <- addCmdOpts ["-Wall"] df
        return $ G.warningFlags df'

setCabalPkg :: DynFlags -> DynFlags
setCabalPkg dflag = gopt_set dflag Opt_BuildingCabalPackage

addPackageFlags :: [Package] -> DynFlags -> DynFlags
addPackageFlags pkgs df =
    df { packageFlags = packageFlags df ++ expose `map` pkgs }
  where
    expose pkg = ExposePackage pkgid (PackageArg name) (ModRenaming True [])
      where
        (name,_,_) = pkg
        pkgid = showPkgId pkg
