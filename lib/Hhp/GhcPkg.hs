{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Hhp.GhcPkg (
    ghcPkgList,
    ghcPkgListEx,
    ghcPkgDbOpt,
    ghcPkgDbStackOpts,
    ghcDbStackOpts,
    ghcDbOpt,
    getSandboxDb,
    getPackageDbStack,
) where

import GHC.Settings.Config (cProjectVersionInt) -- ghc version

import Control.Exception (SomeException (..))
import qualified Control.Exception as E
import Data.Char (isAlphaNum, isSpace)
import Data.List (dropWhileEnd, intercalate, isPrefixOf)
import Data.Maybe (listToMaybe, maybeToList)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import Text.ParserCombinators.ReadP (
    ReadP,
    between,
    char,
    choice,
    eof,
    many1,
    sepBy1,
    string,
 )
import qualified Text.ParserCombinators.ReadP as P

import Hhp.Types

ghcVersion :: Int
ghcVersion = read cProjectVersionInt

-- | Get path to sandbox package db
getSandboxDb
    :: FilePath
    -- ^ Path to the cabal package root directory
    -- (containing the @cabal.sandbox.config@ file)
    -> IO FilePath
getSandboxDb cdir = getSandboxDbDir (cdir </> "cabal.sandbox.config")

-- | Extract the sandbox package db directory from the cabal.sandbox.config file.
--   Exception is thrown if the sandbox config file is broken.
getSandboxDbDir
    :: FilePath
    -- ^ Path to the @cabal.sandbox.config@ file
    -> IO FilePath
getSandboxDbDir sconf = do
    -- Be strict to ensure that an error can be caught.
    !path <- extractValue . parse <$> readFile sconf
    return path
  where
    key = "package-db:"
    keyLen = length key

    parse = unsafeHead . filter (key `isPrefixOf`) . lines
    extractValue = dropWhileEnd isSpace . dropWhile isSpace . drop keyLen

getPackageDbStack
    :: FilePath
    -- ^ Project Directory (where the
    -- cabal.sandbox.config file would be if it
    -- exists)
    -> IO [GhcPkgDb]
getPackageDbStack cdir =
    (getSandboxDb cdir >>= \db -> return [GlobalDb, PackageDb db])
        `E.catch` \(_ :: SomeException) -> return [GlobalDb, UserDb]

-- | List packages in one or more ghc package store
ghcPkgList :: [GhcPkgDb] -> IO [PackageBaseName]
ghcPkgList dbs = map fst3 <$> ghcPkgListEx dbs
  where
    fst3 (x, _, _) = x

ghcPkgListEx :: [GhcPkgDb] -> IO [Package]
ghcPkgListEx dbs = do
    (rv, output, err) <- readProcessWithExitCode "ghc-pkg" opts ""
    case rv of
        ExitFailure val -> do
            hPutStrLn stderr err
            fail $ "ghc-pkg " ++ unwords opts ++ " (exit " ++ show val ++ ")"
        ExitSuccess -> return ()

    return $ parseGhcPkgOutput $ lines output
  where
    opts = ["list", "-v"] ++ ghcPkgDbStackOpts dbs

parseGhcPkgOutput :: [String] -> [Package]
parseGhcPkgOutput [] = []
parseGhcPkgOutput (l : ls) =
    parseGhcPkgOutput ls ++ case l of
        [] -> []
        h : _
            | isSpace h -> maybeToList $ packageLine l
            | otherwise -> []

packageLine :: String -> Maybe Package
packageLine l =
    case listToMaybe $ P.readP_to_S packageLineP l of
        Just ((Normal, p), _) -> Just p
        Just ((Hidden, p), _) -> Just p
        _ -> Nothing

data PackageState = Normal | Hidden | Broken deriving (Eq, Show)

packageLineP :: ReadP (PackageState, Package)
packageLineP = do
    P.skipSpaces
    p <-
        choice
            [ (Hidden,) <$> between (char '(') (char ')') packageP
            , (Broken,) <$> between (char '{') (char '}') packageP
            , (Normal,) <$> packageP
            ]
    eof
    return p

packageP :: ReadP (PackageBaseName, PackageVersion, PackageId)
packageP = do
    pkgSpec@(name, ver) <- packageSpecP
    P.skipSpaces
    i <- between (char '(') (char ')') $ packageIdSpecP pkgSpec
    return (name, ver, i)

packageSpecP :: ReadP (PackageBaseName, PackageVersion)
packageSpecP = do
    fs <- many1 packageCompCharP `sepBy1` char '-'
    return (intercalate "-" (init fs), last fs)

packageIdSpecP :: (PackageBaseName, PackageVersion) -> ReadP PackageId
packageIdSpecP (name, ver) = do
    _ <- string name >> char '-' >> string ver
    choice
        [ char '-' >> many1 (P.satisfy isAlphaNum)
        , return ""
        ]

packageCompCharP :: ReadP Char
packageCompCharP =
    P.satisfy $ \c -> isAlphaNum c || c `elem` "_-."

-- | Get options needed to add a list of package dbs to ghc-pkg's db stack
ghcPkgDbStackOpts
    :: [GhcPkgDb]
    -- ^ Package db stack
    -> [String]
ghcPkgDbStackOpts dbs = ghcPkgDbOpt `concatMap` dbs

-- | Get options needed to add a list of package dbs to ghc's db stack
ghcDbStackOpts
    :: [GhcPkgDb]
    -- ^ Package db stack
    -> [String]
ghcDbStackOpts dbs = ghcDbOpt `concatMap` dbs

ghcPkgDbOpt :: GhcPkgDb -> [String]
ghcPkgDbOpt GlobalDb = ["--global"]
ghcPkgDbOpt UserDb = ["--user"]
ghcPkgDbOpt (PackageDb pkgDb)
    | ghcVersion < 706 = ["--no-user-package-conf", "--package-conf=" ++ pkgDb]
    | otherwise = ["--no-user-package-db", "--package-db=" ++ pkgDb]

ghcDbOpt :: GhcPkgDb -> [String]
ghcDbOpt GlobalDb
    | ghcVersion < 706 = ["-global-package-conf"]
    | otherwise = ["-global-package-db"]
ghcDbOpt UserDb
    | ghcVersion < 706 = ["-user-package-conf"]
    | otherwise = ["-user-package-db"]
ghcDbOpt (PackageDb pkgDb)
    | ghcVersion < 706 = ["-no-user-package-conf", "-package-conf", pkgDb]
    | otherwise = ["-no-user-package-db", "-package-db", pkgDb]
