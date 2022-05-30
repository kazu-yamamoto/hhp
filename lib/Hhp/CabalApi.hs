{-# LANGUAGE OverloadedStrings, CPP #-}

module Hhp.CabalApi (
    getCompilerOptions
  , parseCabalFile
  , cabalAllBuildInfo
  , cabalDependPackages
  , cabalSourceDirs
  , cabalAllTargets
  ) where

import Distribution.Compiler (unknownCompilerInfo, AbiTag(NoAbiTag))
import Distribution.ModuleName (ModuleName,toFilePath)
import Distribution.Package (Dependency(Dependency))
import qualified Distribution.Package as C
import Distribution.PackageDescription (PackageDescription, BuildInfo, TestSuite, TestSuiteInterface(..), Executable)
import qualified Distribution.PackageDescription as P
import Distribution.Simple.Compiler (CompilerId(..), CompilerFlavor(..))
import Distribution.Simple.Program (ghcProgram)
import Distribution.Simple.Program.Types (programName, programFindVersion)
import Distribution.System (buildPlatform)
import Distribution.Text (display)
import Distribution.Verbosity (silent)
import Distribution.Version (Version)

#if MIN_VERSION_Cabal(3,2,0)
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Types.ComponentRequestedSpec (defaultComponentRequestedSpec)
import Distribution.Types.Flag (mkFlagAssignment, mkFlagName)
import Distribution.Types.PackageName (unPackageName)
#elif MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Types.ComponentRequestedSpec (defaultComponentRequestedSpec)
import Distribution.Types.GenericPackageDescription (mkFlagAssignment, mkFlagName)
import Distribution.Types.PackageName (unPackageName)
#elif MIN_VERSION_Cabal(2,0,0)
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Types.ComponentRequestedSpec (defaultComponentRequestedSpec)
import Distribution.Types.GenericPackageDescription (mkFlagName)
import Distribution.Types.PackageName (unPackageName)
#else
import Distribution.Package (PackageName(PackageName))
import Distribution.PackageDescription (FlagName (FlagName))
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
#endif

import Control.Exception (throwIO)
import Control.Monad (filterM)
import CoreMonad (liftIO)
import Data.Maybe (maybeToList, mapMaybe)
import Data.Set (fromList, toList)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath (dropExtension, takeFileName, (</>))

import Hhp.Types
import Hhp.GhcPkg

----------------------------------------------------------------

-- | Getting necessary 'CompilerOptions' from three information sources.
getCompilerOptions :: [GHCOption]
                   -> Cradle
                   -> PackageDescription
                   -> IO CompilerOptions
getCompilerOptions ghcopts cradle pkgDesc = do
    gopts <- getGHCOptions ghcopts cradle rdir $ head buildInfos
    dbPkgs <- ghcPkgListEx (cradlePkgDbStack cradle)
    return $ CompilerOptions gopts idirs (depPkgs dbPkgs)
  where
    wdir       = cradleCurrentDir cradle
    rdir       = cradleRootDir    cradle
    Just cfile = cradleCabalFile  cradle
    thisPkg    = dropExtension $ takeFileName cfile
    buildInfos = cabalAllBuildInfo pkgDesc
    idirs      = includeDirectories rdir wdir $ cabalSourceDirs buildInfos
    depPkgs ps = attachPackageIds ps
                   $ removeThem (problematicPackages ++ [thisPkg])
                   $ cabalDependPackages buildInfos

----------------------------------------------------------------
-- Dependent packages

removeThem :: [PackageBaseName] -> [PackageBaseName] -> [PackageBaseName]
removeThem badpkgs = filter (`notElem` badpkgs)

problematicPackages :: [PackageBaseName]
problematicPackages = [
    "base-compat" -- providing "Prelude"
  ]

attachPackageIds :: [Package] -> [PackageBaseName] -> [Package]
attachPackageIds pkgs = mapMaybe (`lookup3` pkgs)

lookup3 :: Eq a => a -> [(a,b,c)] -> Maybe (a,b,c)
lookup3 _ [] = Nothing
lookup3 k (t@(a,_,_):ls)
    | k == a = Just t
    | otherwise = lookup3 k ls

----------------------------------------------------------------
-- Include directories for modules

cabalBuildDirs :: [FilePath]
cabalBuildDirs = ["dist/build", "dist/build/autogen"]

includeDirectories :: FilePath -> FilePath -> [FilePath] -> [FilePath]
includeDirectories cdir wdir dirs = uniqueAndSort (extdirs ++ [cdir,wdir])
  where
    extdirs = map expand $ dirs ++ cabalBuildDirs
    expand "."    = cdir
    expand subdir = cdir </> subdir

----------------------------------------------------------------

-- | Parsing a cabal file and returns 'PackageDescription'.
--   'IOException' is thrown if parsing fails.
parseCabalFile :: FilePath -> IO PackageDescription
parseCabalFile file = do
    cid <- getGHCId
    let cid' = unknownCompilerInfo cid NoAbiTag
#if MIN_VERSION_Cabal(2,2,0)
    epgd <- readGenericPackageDescription silent file
#else
    epgd <- readPackageDescription silent file
#endif
    flags <- getFlags
    case toPkgDesc cid' flags epgd of
        Left deps    -> throwIO $ userError $ show deps ++ " are not installed"
        Right (pd,_) -> if nullPkg pd
                        then throwIO $ userError $ file ++ " is broken"
                        else return pd
  where
    envFlags = do
      let parseF []      = []
          parseF ccs@(c:cs)
            | c == '-'   = [(mkFlagName cs, False)]
            | otherwise  = [(mkFlagName ccs, True)]
      maybe [] (concatMap parseF . words) `fmap` lookupEnv "HHP_CABAL_FLAGS"
#if MIN_VERSION_Cabal(2,2,0)
    getFlags = mkFlagAssignment `fmap` envFlags
#else
    getFlags = envFlags
#endif
#if MIN_VERSION_Cabal(2,0,0)
    nullPkg pd = unPackageName (C.pkgName (P.package pd)) == ""
    toPkgDesc cid flags = finalizePD flags defaultComponentRequestedSpec (const True) buildPlatform cid []
#else
    mkFlagName = FlagName
    nullPkg pd = name == ""
      where
        PackageName name = C.pkgName (P.package pd)
    toPkgDesc cid flags = finalizePackageDescription flags (const True) buildPlatform cid []
#endif

----------------------------------------------------------------

getGHCOptions :: [GHCOption] -> Cradle -> FilePath -> BuildInfo -> IO [GHCOption]
getGHCOptions ghcopts cradle rdir binfo = do
    cabalCpp <- cabalCppOptions rdir
    let cpps = map ("-optP" ++) $ P.cppOptions binfo ++ cabalCpp
    return $ ghcopts ++ pkgDb ++ exts ++ [lang] ++ libs ++ libDirs ++ cpps
  where
    pkgDb = ghcDbStackOpts $ cradlePkgDbStack cradle
    lang = maybe "-XHaskell98" (("-X" ++) . display) $ P.defaultLanguage binfo
    libDirs = map ("-L" ++) $ P.extraLibDirs binfo
    exts = map (("-X" ++) . display) $ P.usedExtensions binfo
    libs = map ("-l" ++) $ P.extraLibs binfo

cabalCppOptions :: FilePath -> IO [String]
cabalCppOptions dir = do
    exist <- doesFileExist cabalMacro
    return $ if exist then
        ["-include", cabalMacro]
      else
        []
  where
    cabalMacro = dir </> "dist/build/autogen/cabal_macros.h"

----------------------------------------------------------------

-- | Extracting all 'BuildInfo' for libraries, executables, and tests.
cabalAllBuildInfo :: PackageDescription -> [BuildInfo]
cabalAllBuildInfo pd = libBI ++ subBI ++ execBI ++ testBI ++ benchBI
  where
    libBI   = map P.libBuildInfo       $ maybeToList $ P.library pd
#if MIN_VERSION_Cabal(2,0,0)
    subBI   = map P.libBuildInfo       $ P.subLibraries pd
#else
    subBI   = []
#endif
    execBI  = map P.buildInfo          $ P.executables pd
    testBI  = map P.testBuildInfo      $ P.testSuites pd
#if __GLASGOW_HASKELL__ >= 704
    benchBI = map P.benchmarkBuildInfo $ P.benchmarks pd
#else
    benchBI = []
#endif

----------------------------------------------------------------

-- | Extracting package names of dependency.
cabalDependPackages :: [BuildInfo] -> [PackageBaseName]
cabalDependPackages bis = uniqueAndSort pkgs
  where
    pkgs = map getDependencyPackageName $ concatMap P.targetBuildDepends bis
#if MIN_VERSION_Cabal(3,0,0)
    getDependencyPackageName (Dependency pkg _ _) = unPackageName pkg
#elif MIN_VERSION_Cabal(2,0,0)
    getDependencyPackageName (Dependency pkg _) = unPackageName pkg
#else
    getDependencyPackageName (Dependency (PackageName nm) _) = nm
#endif

----------------------------------------------------------------

-- | Extracting include directories for modules.
cabalSourceDirs :: [BuildInfo] -> [IncludeDir]
cabalSourceDirs bis = uniqueAndSort $ concatMap P.hsSourceDirs bis

----------------------------------------------------------------

uniqueAndSort :: [String] -> [String]
uniqueAndSort = toList . fromList

----------------------------------------------------------------

getGHCId :: IO CompilerId
getGHCId = CompilerId GHC <$> getGHC

getGHC :: IO Version
getGHC = do
    mv <- programFindVersion ghcProgram silent (programName ghcProgram)
    case mv of
        Nothing -> throwIO $ userError "ghc not found"
        Just v  -> return v

----------------------------------------------------------------

-- | Extracting all 'Module' 'FilePath's for libraries, executables,
-- tests and benchmarks.
cabalAllTargets :: PackageDescription -> IO ([String],[String],[String],[String])
cabalAllTargets pd = do
    exeTargets  <- mapM getExecutableTarget $ P.executables pd
    testTargets <- mapM getTestTarget $ P.testSuites pd
    return (libTargets,concat exeTargets,concat testTargets,benchTargets)
  where
    lib = case P.library pd of
            Nothing -> []
#if MIN_VERSION_Cabal(2,0,0)
            Just l -> P.explicitLibModules l
#else
            Just l -> P.libModules l
#endif

    libTargets = map toModuleString lib
#if __GLASGOW_HASKELL__ >= 704
    benchTargets = map toModuleString $ concatMap P.benchmarkModules $ P.benchmarks  pd
#else
    benchTargets = []
#endif
    toModuleString :: ModuleName -> String
    toModuleString mn = fromFilePath $ toFilePath mn

    fromFilePath :: FilePath -> String
    fromFilePath fp = map (\c -> if c=='/' then '.' else c) fp

    getTestTarget :: TestSuite -> IO [String]
    getTestTarget ts =
       case P.testInterface ts of
        (TestSuiteExeV10 _ filePath) -> do
          let maybeTests = [p </> e | p <- P.hsSourceDirs $ P.testBuildInfo ts, e <- [filePath]]
          liftIO $ filterM doesFileExist maybeTests
        (TestSuiteLibV09 _ moduleName) -> return [toModuleString moduleName]
        (TestSuiteUnsupported _)       -> return []

    getExecutableTarget :: Executable -> IO [String]
    getExecutableTarget exe = do
      let maybeExes = [p </> e | p <- P.hsSourceDirs $ P.buildInfo exe, e <- [P.modulePath exe]]
      liftIO $ filterM doesFileExist maybeExes
