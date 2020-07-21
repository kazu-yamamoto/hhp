module LintSpec where

import Test.Hspec

import Hhp

spec :: Spec
spec = do
    describe "lintSyntax" $ do
        it "check syntax with HLint" $ do
            res <- lintSyntax defaultOptions "test/data/hlint.hs"
            res `shouldBe` "test/data/hlint.hs:5:16-26: Warning: Use sum\NULFound:\NUL  foldr (+) 0\NULPerhaps:\NUL  sum\n"

        context "without suggestions" $ do
            it "doesn't output empty line" $ do
                res <- lintSyntax defaultOptions "test/data/hhp-check/Data/Foo.hs"
                res `shouldBe` ""
