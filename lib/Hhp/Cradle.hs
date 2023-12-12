module Hhp.Cradle (
    findCradle,
    findCradleWithoutSandbox,
) where

import Control.Applicative ((<|>))
import qualified Control.Exception as E
import Control.Monad (filterM)
import Data.List (isSuffixOf)
import System.Directory (
    doesFileExist,
    getCurrentDirectory,
    getDirectoryContents,
 )
import System.FilePath (takeDirectory, (</>))

import Hhp.GhcPkg
import Hhp.Types

----------------------------------------------------------------

-- | Finding 'Cradle'.
--   Find a cabal file by tracing ancestor directories.
--   Find a sandbox according to a cabal sandbox config
--   in a cabal directory.
findCradle :: IO Cradle
findCradle = do
    wdir <- getCurrentDirectory
    cabalCradle wdir <|> sandboxCradle wdir <|> plainCradle wdir

cabalCradle :: FilePath -> IO Cradle
cabalCradle wdir = do
    (rdir, cfile) <- cabalDir wdir
    pkgDbStack <- getPackageDbStack rdir
    return
        Cradle
            { cradleCurrentDir = wdir
            , cradleRootDir = rdir
            , cradleCabalFile = Just cfile
            , cradlePkgDbStack = pkgDbStack
            }

sandboxCradle :: FilePath -> IO Cradle
sandboxCradle wdir = do
    rdir <- getSandboxDir wdir
    pkgDbStack <- getPackageDbStack rdir
    return
        Cradle
            { cradleCurrentDir = wdir
            , cradleRootDir = rdir
            , cradleCabalFile = Nothing
            , cradlePkgDbStack = pkgDbStack
            }

plainCradle :: FilePath -> IO Cradle
plainCradle wdir =
    return
        Cradle
            { cradleCurrentDir = wdir
            , cradleRootDir = wdir
            , cradleCabalFile = Nothing
            , cradlePkgDbStack = [GlobalDb]
            }

-- Just for testing
findCradleWithoutSandbox :: IO Cradle
findCradleWithoutSandbox = do
    cradle <- findCradle
    return cradle{cradlePkgDbStack = [GlobalDb]}

----------------------------------------------------------------

cabalSuffix :: String
cabalSuffix = ".cabal"

cabalSuffixLength :: Int
cabalSuffixLength = length cabalSuffix

-- Finding a Cabal file up to the root directory
-- Input: a directly to investigate
-- Output: (the path to the directory containing a Cabal file
--         ,the path to the Cabal file)
cabalDir :: FilePath -> IO (FilePath, FilePath)
cabalDir dir = do
    cnts <- getCabalFiles dir
    case cnts of
        []
            | dir' == dir -> E.throwIO $ userError "cabal files not found"
            | otherwise -> cabalDir dir'
        cfile : _ -> return (dir, dir </> cfile)
  where
    dir' = takeDirectory dir

getCabalFiles :: FilePath -> IO [FilePath]
getCabalFiles dir = getFiles >>= filterM doesCabalFileExist
  where
    isCabal name =
        cabalSuffix `isSuffixOf` name
            && length name > cabalSuffixLength
    getFiles = filter isCabal <$> getDirectoryContents dir
    doesCabalFileExist file = doesFileExist $ dir </> file

----------------------------------------------------------------

getSandboxDir :: FilePath -> IO FilePath
getSandboxDir dir = do
    exist <- doesFileExist sfile
    if exist
        then return dir
        else
            if dir == dir'
                then E.throwIO $ userError "sandbox not found"
                else getSandboxDir dir'
  where
    sfile = dir </> "cabal.sandbox.config"
    dir' = takeDirectory dir
