module FlagSpec where

import Test.Hspec

import Hhp

spec :: Spec
spec = do
    describe "listFlags" $ do
        it "lists up GHC flags" $ do
            flags <- lines <$> listFlags defaultOptions
            flags `shouldContain` ["-Wno-orphans"]
