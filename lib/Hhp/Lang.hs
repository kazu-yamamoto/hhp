module Hhp.Lang where

import DynFlags (supportedLanguagesAndExtensions)

import Hhp.Types

-- | Listing language extensions.

listLanguages :: Options -> IO String
listLanguages opt = return $ convert opt supportedLanguagesAndExtensions
