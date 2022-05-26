module Hhp.Lang where

import Hhp.Gap
import Hhp.Types

-- | Listing language extensions.
listLanguages :: Options -> IO String
listLanguages opt = return $ convert opt languagesAndExtensions
