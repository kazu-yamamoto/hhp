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
import Hhp.Gap
import Hhp.Info
import Hhp.List

import GHC (runGhc)
import CoreMonad (liftIO)
import GHC (Ghc)
import qualified GHC as G

import Data.Maybe (fromMaybe)

getMainFileToBeDeleted :: FilePath -> Ghc (Maybe FilePath)
getMainFileToBeDeleted file = isSameMainFile file <$> getModSummaryForMain

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
