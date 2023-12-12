module Hhp.Flag where

import GHC.Driver.Session (fFlags, fLangFlags, flagSpecName, wWarningFlags)

import Hhp.Types

-- | Listing GHC flags. (e.g -Wno-orphans)
listFlags :: Options -> IO String
listFlags opt = return $ convert opt options
  where
    options = expand "-f" fOptions ++ expand "-W" wOptions
    fOptions = map flagSpecName fFlags ++ map flagSpecName fLangFlags
    wOptions = map flagSpecName wWarningFlags
    expand prefix lst =
        [ prefix ++ no ++ option
        | option <- lst
        , no <- ["", "no-"]
        ]
