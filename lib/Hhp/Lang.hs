module Hhp.Lang where

import GHC.Driver.Session (supportedLanguagesAndExtensions)
import GHC.Platform.Host (cHostPlatformMini)

import Hhp.Types

-- | Listing language extensions.
listLanguages :: Options -> IO String
listLanguages opt = return $ convert opt $ supportedLanguagesAndExtensions cHostPlatformMini
