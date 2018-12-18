module ListSpec where

import Test.Hspec

import Hhp

spec :: Spec
spec = do
    describe "listModules" $ do
        it "lists up module names" $ do
            cradle <- findCradle
            modules <- lines <$> listModules defaultOptions cradle
            modules `shouldContain` ["Data.Map"]
