{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Hhp.CabalApi (
    getCompilerOptions,
    parseCabalFile,
    cabalAllBuildInfo,
    cabalDependPackages,
    cabalSourceDirs,
    cabalAllTargets,
) where

import Distribution.Compiler (AbiTag (NoAbiTag), unknownCompilerInfo)
import Distribution.ModuleName (ModuleName, toFilePath)
import Distribution.Package (Dependency (Dependency))
import qualified Distribution.Package as C
import Distribution.PackageDescription (
    BuildInfo,
    Executable,
    PackageDescription,
    TestSuite,
    TestSuiteInterface (..),
 )
import qualified Distribution.PackageDescription as P
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.Simple.Compiler (CompilerFlavor (..), CompilerId (..))
import Distribution.Simple.Program (ghcProgram)
import Distribution.Simple.Program.Types (programFindVersion, programName)
import Distribution.System (buildPlatform)
import Distribution.Text (display)
import Distribution.Verbosity (silent)
import Distribution.Version (Version)
#if MIN_VERSION_Cabal(3,8,0)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
#else
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
#endif
import Distribution.Types.ComponentRequestedSpec (defaultComponentRequestedSpec)
import Distribution.Types.Flag (mkFlagAssignment, mkFlagName)
import Distribution.Types.PackageName (unPackageName)
#if MIN_VERSION_Cabal(3,6,0)
import Distribution.Utils.Path (getSymbolicPath, SymbolicPath)
#endif
#if MIN_VERSION_Cabal(3,14,0)
import qualified Distribution.Utils.Path as Path
#endif

import GHC.Utils.Monad (liftIO)

import Control.Exception (throwIO)
import Control.Monad (filterM)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Set (fromList, toList)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath (dropExtension, takeFileName, (</>))

import Hhp.GhcPkg
import Hhp.Types

----------------------------------------------------------------

-- | Getting necessary 'CompilerOptions' from three information sources.
getCompilerOptions
    :: [GHCOption]
    -> Cradle
    -> PackageDescription
    -> IO CompilerOptions
getCompilerOptions ghcopts cradle pkgDesc = do
    gopts <- getGHCOptions ghcopts cradle rdir $ unsafeHead buildInfos
    dbPkgs <- ghcPkgListEx (cradlePkgDbStack cradle)
    return $ CompilerOptions gopts idirs (depPkgs dbPkgs)
  where
    wdir = cradleCurrentDir cradle
    rdir = cradleRootDir cradle
    cfile = fromMaybe "error getCompilerOptions" $ cradleCabalFile cradle
    thisPkg = dropExtension $ takeFileName cfile
    buildInfos = cabalAllBuildInfo pkgDesc
    idirs = includeDirectories rdir wdir $ cabalSourceDirs buildInfos
    depPkgs ps =
        attachPackageIds ps $
            removeThem (problematicPackages ++ [thisPkg]) $
                cabalDependPackages buildInfos

----------------------------------------------------------------
-- Dependent packages

removeThem :: [PackageBaseName] -> [PackageBaseName] -> [PackageBaseName]
removeThem badpkgs = filter (`notElem` badpkgs)

problematicPackages :: [PackageBaseName]
problematicPackages =
    [ "base-compat" -- providing "Prelude"
    ]

attachPackageIds :: [Package] -> [PackageBaseName] -> [Package]
attachPackageIds pkgs = mapMaybe (`lookup3` pkgs)

lookup3 :: Eq a => a -> [(a, b, c)] -> Maybe (a, b, c)
lookup3 _ [] = Nothing
lookup3 k (t@(a, _, _) : ls)
    | k == a = Just t
    | otherwise = lookup3 k ls

----------------------------------------------------------------
-- Include directories for modules

cabalBuildDirs :: [FilePath]
cabalBuildDirs = ["dist/build", "dist/build/autogen"]

includeDirectories :: FilePath -> FilePath -> [FilePath] -> [FilePath]
includeDirectories cdir wdir dirs = uniqueAndSort (extdirs ++ [cdir, wdir])
  where
    extdirs = map expand $ dirs ++ cabalBuildDirs
    expand "." = cdir
    expand subdir = cdir </> subdir

----------------------------------------------------------------

-- | Parsing a cabal file and returns 'PackageDescription'.
--   'IOException' is thrown if parsing fails.
parseCabalFile :: FilePath -> IO PackageDescription
parseCabalFile file = do
    cid <- getGHCId
    let cid' = unknownCompilerInfo cid NoAbiTag
    epgd <- readPackageDescription file
    flags <- getFlags
    case toPkgDesc cid' flags epgd of
        Left deps -> throwIO $ userError $ show deps ++ " are not installed"
        Right (pd, _) ->
            if nullPkg pd
                then throwIO $ userError $ file ++ " is broken"
                else return pd
  where
    envFlags = do
        let parseF [] = []
            parseF ccs@(c : cs)
                | c == '-' = [(mkFlagName cs, False)]
                | otherwise = [(mkFlagName ccs, True)]
        maybe [] (concatMap parseF . words) `fmap` lookupEnv "HHP_CABAL_FLAGS"
    getFlags = mkFlagAssignment <$> envFlags
    nullPkg pd = unPackageName (C.pkgName (P.package pd)) == ""
    toPkgDesc cid flags =
        finalizePD flags defaultComponentRequestedSpec (const True) buildPlatform cid []

----------------------------------------------------------------

getGHCOptions
    :: [GHCOption] -> Cradle -> FilePath -> BuildInfo -> IO [GHCOption]
getGHCOptions ghcopts cradle rdir binfo = do
    cabalCpp <- cabalCppOptions rdir
    let cpps = map ("-optP" ++) $ P.cppOptions binfo ++ cabalCpp
    return $ ghcopts ++ pkgDb ++ exts ++ [lang] ++ libs ++ libDirs ++ cpps
  where
    pkgDb = ghcDbStackOpts $ cradlePkgDbStack cradle
    lang = maybe "-XHaskell98" (("-X" ++) . display) $ P.defaultLanguage binfo
    libDirs = map ("-L" ++) $ extLibDirs binfo
    exts = map (("-X" ++) . display) $ P.usedExtensions binfo
    libs = map ("-l" ++) $ P.extraLibs binfo

cabalCppOptions :: FilePath -> IO [String]
cabalCppOptions dir = do
    exist <- doesFileExist cabalMacro
    return $
        if exist
            then ["-include", cabalMacro]
            else []
  where
    cabalMacro = dir </> "dist/build/autogen/cabal_macros.h"

----------------------------------------------------------------

-- | Extracting all 'BuildInfo' for libraries, executables, and tests.
cabalAllBuildInfo :: PackageDescription -> [BuildInfo]
cabalAllBuildInfo pd = libBI ++ subBI ++ execBI ++ testBI ++ benchBI
  where
    libBI = map P.libBuildInfo $ maybeToList $ P.library pd
    subBI = map P.libBuildInfo $ P.subLibraries pd
    execBI = map P.buildInfo $ P.executables pd
    testBI = map P.testBuildInfo $ P.testSuites pd
    benchBI = map P.benchmarkBuildInfo $ P.benchmarks pd

----------------------------------------------------------------

-- | Extracting package names of dependency.
cabalDependPackages :: [BuildInfo] -> [PackageBaseName]
cabalDependPackages bis = uniqueAndSort pkgs
  where
    pkgs = map getDependencyPackageName $ concatMap P.targetBuildDepends bis
    getDependencyPackageName (Dependency pkg _ _) = unPackageName pkg

----------------------------------------------------------------

-- | Extracting include directories for modules.
cabalSourceDirs :: [BuildInfo] -> [IncludeDir]
cabalSourceDirs bis = uniqueAndSort $ concatMap (map toPath . P.hsSourceDirs) bis

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
        Just v -> return v

----------------------------------------------------------------

-- | Extracting all 'Module' 'FilePath's for libraries, executables,
-- tests and benchmarks.
cabalAllTargets
    :: PackageDescription -> IO ([String], [String], [String], [String])
cabalAllTargets pd = do
    exeTargets <- mapM getExecutableTarget $ P.executables pd
    testTargets <- mapM getTestTarget $ P.testSuites pd
    return (libTargets, concat exeTargets, concat testTargets, benchTargets)
  where
    lib = maybe [] P.explicitLibModules $ P.library pd

    libTargets = map toModuleString lib
    benchTargets = map toModuleString $ concatMap P.benchmarkModules $ P.benchmarks pd
    toModuleString :: ModuleName -> String
    toModuleString mn = fromFilePath $ toFilePath mn

    fromFilePath :: FilePath -> String
    fromFilePath fp = map (\c -> if c == '/' then '.' else c) fp

    getTestTarget :: TestSuite -> IO [String]
    getTestTarget ts =
        case P.testInterface ts of
            (TestSuiteExeV10 _ filePath) -> do
                let maybeTests =
                        [ p <//> e
                        | p <- P.hsSourceDirs $ P.testBuildInfo ts
                        , e <- [filePath]
                        ]
                liftIO $ filterM doesFileExist maybeTests
            (TestSuiteLibV09 _ moduleName) -> return [toModuleString moduleName]
            (TestSuiteUnsupported _) -> return []

    getExecutableTarget :: Executable -> IO [String]
    getExecutableTarget exe = do
        let maybeExes =
                [ p <//> e
                | p <- P.hsSourceDirs $ P.buildInfo exe
                , e <- [P.modulePath exe]
                ]
        liftIO $ filterM doesFileExist maybeExes

#if MIN_VERSION_Cabal(3,14,0)
(<//>) :: SymbolicPath Path.Pkg (Path.Dir Path.Source)
       -> Path.RelativePath Path.Source Path.File
       -> FilePath
dir <//> file = toPath (dir Path.</> file)
#else
(<//>) :: SymbolicPath from to -> FilePath -> FilePath
dir <//> file = toPath dir </> file
#endif

extLibDirs :: BuildInfo -> [FilePath]
#if MIN_VERSION_Cabal(3,14,0)
extLibDirs = map getSymbolicPath . P.extraLibDirs
#else
extLibDirs = P.extraLibDirs
#endif

#if MIN_VERSION_Cabal(3,6,0)
toPath :: SymbolicPath from to -> FilePath
toPath = getSymbolicPath
#else
toPath :: String -> String
toPath = id
#endif

readPackageDescription :: FilePath -> IO P.GenericPackageDescription
#if MIN_VERSION_Cabal(3,14,0)
readPackageDescription = readGenericPackageDescription silent Nothing . Path.makeSymbolicPath
#else
readPackageDescription = readGenericPackageDescription silent
#endif
