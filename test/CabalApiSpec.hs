{-# LANGUAGE ScopedTypeVariables #-}

module CabalApiSpec where

import Control.Exception
import Data.Maybe
import System.Directory
import System.Environment (unsetEnv, setEnv)
import System.FilePath
import Test.Hspec

import Hhp.CabalApi
import Hhp.Cradle
import Hhp.Types

import Dir

import Config (cProjectVersionInt) -- ghc version

ghcVersion :: Int
ghcVersion = read cProjectVersionInt


spec :: Spec
spec = do
    describe "parseCabalFile" $ do
        it "throws an exception if the cabal file is broken" $ do
            parseCabalFile "test/data/broken-cabal/broken.cabal" `shouldThrow` (\(_::IOException) -> True)

    describe "getCompilerOptions" $ do
        it "gets necessary CompilerOptions" $ do
            cwd <- getCurrentDirectory
            withDirectory "test/data/subdir1/subdir2" $ \dir -> do
                cradle <- findCradle
                pkgDesc <- parseCabalFile $ fromJust $ cradleCabalFile cradle
                res <- getCompilerOptions [] cradle pkgDesc
                let res' = res {
                        ghcOptions  = ghcOptions res
                      , includeDirs = map (toRelativeDir dir) (includeDirs res)
                      }
                if ghcVersion < 706
                  then ghcOptions res' `shouldBe` ["-global-package-conf", "-no-user-package-conf","-package-conf",cwd </> "test/data/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d","-XHaskell98"]
                  else ghcOptions res' `shouldBe` ["-global-package-db", "-no-user-package-db","-package-db",cwd </> "test/data/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d","-XHaskell98"]
                includeDirs res' `shouldBe` ["test/data","test/data/dist/build","test/data/dist/build/autogen","test/data/subdir1/subdir2","test/data/test"]
                depPackages res' `shouldSatisfy` (("Cabal", "1.18.1.3", "2b161c6bf77657aa17e1681d83cb051b")`elem`)


    describe "cabalDependPackages" $ do
        it "extracts dependent packages" $ do
            pkgs <- cabalDependPackages . cabalAllBuildInfo <$> parseCabalFile "test/data/cabalapi.cabal"
            pkgs `shouldBe` ["Cabal","base","template-haskell"]

    describe "cabalSourceDirs" $ do
        it "extracts all hs-source-dirs" $ do
            dirs <- cabalSourceDirs . cabalAllBuildInfo <$> parseCabalFile "test/data/check-test-subdir/check-test-subdir.cabal"
            dirs `shouldBe` ["src", "test"]
        it "extracts all hs-source-dirs including \".\"" $ do
            dirs <- cabalSourceDirs . cabalAllBuildInfo <$> parseCabalFile "test/data/cabalapi.cabal"
            dirs `shouldBe` [".", "test"]

    describe "cabal-subLibraries" $ do
        it "dependent packages with sublib" $ do
            pkgs <- cabalDependPackages . cabalAllBuildInfo <$> parseCabalFile "test/data/check-sublib/check-sublib.cabal"
            pkgs `shouldBe` ["array", "base", "bytestring"]

    describe "HHP_CABAL_FLAGS" $ do
        it "dependent packages without flags" $ do
            unsetEnv "HHP_CABAL_FLAGS"
            pkgs <- cabalDependPackages . cabalAllBuildInfo <$> parseCabalFile "test/data/check-flags/check-flags.cabal"
            pkgs `shouldBe` ["base", "directory"]
        it "dependent packages with foo flag" $ do
            setEnv "HHP_CABAL_FLAGS" "foo"
            pkgs <- cabalDependPackages . cabalAllBuildInfo <$> parseCabalFile "test/data/check-flags/check-flags.cabal"
            pkgs `shouldBe` ["base", "directory", "filepath"]
        it "dependent packages with foo and -bar flag" $ do
            setEnv "HHP_CABAL_FLAGS" "foo -bar"
            pkgs <- cabalDependPackages . cabalAllBuildInfo <$> parseCabalFile "test/data/check-flags/check-flags.cabal"
            pkgs `shouldBe` ["base", "filepath"]

{-
    describe "cabalAllBuildInfo" $ do
        it "extracts build info" $ do
            info <- cabalAllBuildInfo <$> parseCabalFile "test/data/cabalapi.cabal"
            show info `shouldBe` "[BuildInfo {buildable = True, buildTools = [], cppOptions = [], ccOptions = [], ldOptions = [], pkgconfigDepends = [], frameworks = [], cSources = [], hsSourceDirs = [\".\"], otherModules = [ModuleName [\"Browse\"],ModuleName [\"CabalApi\"],ModuleName [\"Cabal\"],ModuleName [\"CabalDev\"],ModuleName [\"Check\"],ModuleName [\"ErrMsg\"],ModuleName [\"Flag\"],ModuleName [\"GHCApi\"],ModuleName [\"GHCChoice\"],ModuleName [\"Gap\"],ModuleName [\"Info\"],ModuleName [\"Lang\"],ModuleName [\"Lint\"],ModuleName [\"List\"],ModuleName [\"Paths_ghc_mod\"],ModuleName [\"Types\"]], defaultLanguage = Nothing, otherLanguages = [], defaultExtensions = [], otherExtensions = [], oldExtensions = [], extraLibs = [], extraLibDirs = [], includeDirs = [], includes = [], installIncludes = [], options = [(GHC,[\"-Wall\"])], ghcProfOptions = [], ghcSharedOptions = [], customFieldsBI = [], targetBuildDepends = [Dependency (PackageName \"Cabal\") (UnionVersionRanges (ThisVersion (Version {versionBranch = [1,10], versionTags = []})) (LaterVersion (Version {versionBranch = [1,10], versionTags = []}))),Dependency (PackageName \"base\") (IntersectVersionRanges (UnionVersionRanges (ThisVersion (Version {versionBranch = [4,0], versionTags = []})) (LaterVersion (Version {versionBranch = [4,0], versionTags = []}))) (EarlierVersion (Version {versionBranch = [5], versionTags = []}))),Dependency (PackageName \"template-haskell\") AnyVersion]},BuildInfo {buildable = True, buildTools = [], cppOptions = [], ccOptions = [], ldOptions = [], pkgconfigDepends = [], frameworks = [], cSources = [], hsSourceDirs = [\"test\",\".\"], otherModules = [ModuleName [\"Expectation\"],ModuleName [\"BrowseSpec\"],ModuleName [\"CabalApiSpec\"],ModuleName [\"FlagSpec\"],ModuleName [\"LangSpec\"],ModuleName [\"LintSpec\"],ModuleName [\"ListSpec\"]], defaultLanguage = Nothing, otherLanguages = [], defaultExtensions = [], otherExtensions = [], oldExtensions = [], extraLibs = [], extraLibDirs = [], includeDirs = [], includes = [], installIncludes = [], options = [], ghcProfOptions = [], ghcSharedOptions = [], customFieldsBI = [], targetBuildDepends = [Dependency (PackageName \"Cabal\") (UnionVersionRanges (ThisVersion (Version {versionBranch = [1,10], versionTags = []})) (LaterVersion (Version {versionBranch = [1,10], versionTags = []}))),Dependency (PackageName \"base\") (IntersectVersionRanges (UnionVersionRanges (ThisVersion (Version {versionBranch = [4,0], versionTags = []})) (LaterVersion (Version {versionBranch = [4,0], versionTags = []}))) (EarlierVersion (Version {versionBranch = [5], versionTags = []})))]}]"
-}
