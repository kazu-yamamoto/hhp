module Hhp.Check (
    checkSyntax,
    check,
    expandTemplate,
    expand,
    takeRelativePath,
) where

import GHC (DynFlags (..), Ghc)
import GHC.Driver.Session (DumpFlag (Opt_D_dump_splices), dopt_set)

import Hhp.GHCApi
import Hhp.Logger
import Hhp.Types

import Data.List
import System.FilePath

----------------------------------------------------------------

takeRelativePath :: Cradle -> [String] -> Maybe FilePath
takeRelativePath _ [] = Nothing
takeRelativePath cradle (fn : _) = Just rp
  where
    root = cradleRootDir cradle
    rp
        | root `isPrefixOf` fn = takeDirectory $ drop (length root + 1) fn
        | otherwise = takeDirectory fn

----------------------------------------------------------------

-- | Checking syntax of a target file using GHC.
--   Warnings and errors are returned.
checkSyntax
    :: Options
    -> Cradle
    -> [FilePath]
    -- ^ The target files.
    -> IO String
checkSyntax _ _ [] = return ""
checkSyntax opt cradle files = withGHC sessionName $ do
    _ <- initializeFlagsWithCradle opt cradle $ takeRelativePath cradle files
    either id id <$> check opt files
  where
    sessionName = case files of
        [file] -> file
        _ -> "MultipleFiles"

----------------------------------------------------------------

-- | Checking syntax of a target file using GHC.
--   Warnings and errors are returned.
check
    :: Options
    -> [FilePath]
    -- ^ The target files.
    -> Ghc (Either String String)
check opt fileNames =
    withLogger opt (setAllWarningFlags . setPartialSignatures . setDeferTypedHoles) $
        setTargetFiles fileNames

----------------------------------------------------------------

-- | Expanding Haskell Template.
expandTemplate
    :: Options
    -> Cradle
    -> [FilePath]
    -- ^ The target files.
    -> IO String
expandTemplate _ _ [] = return ""
expandTemplate opt cradle files = withGHC sessionName $ do
    _ <- initializeFlagsWithCradle opt cradle $ takeRelativePath cradle files
    either id id <$> expand opt files
  where
    sessionName = case files of
        [file] -> file
        _ -> "MultipleFiles"

----------------------------------------------------------------

-- | Expanding Haskell Template.
expand
    :: Options
    -> [FilePath]
    -- ^ The target files.
    -> Ghc (Either String String)
expand opt fileNames =
    withLogger opt (setDumpSplices . setNoWarningFlags) $
        setTargetFiles fileNames

setDumpSplices :: DynFlags -> DynFlags
setDumpSplices dflag = dopt_set dflag Opt_D_dump_splices
