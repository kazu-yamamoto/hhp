{-# LANGUAGE CPP #-}

-- | The Happy Haskell Programming library.
--   API for interactive processes

module Hhp.Ghc (
  -- * Converting the Ghc monad to the IO monad
    withGHC
  , withGHC'
  -- * Initializing DynFlags
  , initializeFlagsWithCradle
  -- * Ghc utilities
  , boot
  , browse
  , check
  , info
  , types
  , modules
  -- * SymMdlDb
  , Symbol
  , SymMdlDb
  , getSymMdlDb
  , lookupSym
  -- * Misc
  , getSystemLibDir
  , liftIO
  , runGhc
  , getMainFileToBeDeleted
  , Ghc
  ) where

import Hhp.Boot
import Hhp.Browse
import Hhp.Check
import Hhp.Find
import Hhp.GHCApi
import Hhp.Info
import Hhp.List

import GHC (runGhc)
import CoreMonad (liftIO)
import GHC (Ghc)
#if __GLASGOW_HASKELL__ >= 804
import GHC (mgModSummaries)
#endif
import qualified GHC as G

import Data.List (find)
import Data.Maybe (fromMaybe)

getMainFileToBeDeleted :: FilePath -> Ghc (Maybe FilePath)
getMainFileToBeDeleted file = isSameMainFile file <$> getModSummaryForMain

getModSummaryForMain :: Ghc (Maybe G.ModSummary)
#if __GLASGOW_HASKELL__ >= 804
getModSummaryForMain = find isMain . mgModSummaries <$> G.getModuleGraph
#else
getModSummaryForMain = find isMain <$> G.getModuleGraph
#endif
  where
    isMain m = G.moduleNameString (G.moduleName (G.ms_mod m)) == "Main"

isSameMainFile :: FilePath -> Maybe G.ModSummary -> Maybe FilePath
isSameMainFile _    Nothing  = Nothing
isSameMainFile file (Just x)
    | mainfile == file = Nothing
    | otherwise        = Just mainfile
  where
    mmainfile = G.ml_hs_file (G.ms_location x)
    -- G.ms_hspp_file x is a temporary file with CPP.
    -- this is a just fake.
    mainfile = fromMaybe (G.ms_hspp_file x) mmainfile
