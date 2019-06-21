{-# LANGUAGE CPP #-}

module Main where

import Test.DocTest

main :: IO ()
main = doctest [
    "-package"
  , "ghc"
#if MIN_VERSION_hlint(2,1,18)
  , "-hide-package"
  , "ghc-lib-parser"
#endif
  , "-ilib/"
  , "-idist/build/autogen/"
  , "-optP-include"
  , "-optPdist/build/autogen/cabal_macros.h"
  , "lib/Hhp.hs"
  ]
