module LangSpec where

import Test.Hspec

import Hhp

spec :: Spec
spec = do
    describe "listLanguages" $ do
        it "lists up language extensions" $ do
            exts <- lines <$> listLanguages defaultOptions
            exts `shouldContain` ["OverloadedStrings"]
