module Main where

import Test.DocTest

main :: IO ()
main = doctest [
    "-package"
  , "ghc"
  , "-ilib/"
  , "-idist/build/autogen/"
  , "-optP-include"
  , "-optPdist/build/autogen/cabal_macros.h"
  , "lib/Hhp.hs"
  ]
