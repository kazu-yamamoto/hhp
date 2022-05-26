module Hhp.Boot where

import GHC (Ghc)
import GHC.Utils.Monad (liftIO)

import Hhp.Browse
import Hhp.Flag
import Hhp.GHCApi
import Hhp.Lang
import Hhp.List
import Hhp.Types

-- | Printing necessary information for front-end booting.
bootInfo :: Options -> Cradle -> IO String
bootInfo opt cradle = withGHC' $ do
    initializeFlagsWithCradle opt cradle
    boot opt

-- | Printing necessary information for front-end booting.
boot :: Options -> Ghc String
boot opt = do
    mods  <- modules opt
    langs <- liftIO $ listLanguages opt
    flags <- liftIO $ listFlags opt
    pre   <- concat <$> mapM (browse opt) preBrowsedModules
    return $ mods ++ langs ++ flags ++ pre

preBrowsedModules :: [String]
preBrowsedModules = [
    "Prelude"
  , "Control.Applicative"
  , "Control.Exception"
  , "Control.Monad"
  , "Data.Char"
  , "Data.List"
  , "Data.Maybe"
  , "System.IO"
  ]
