{-# LANGUAGE CPP #-}

module LintSpec where

#ifdef HLINT
import Data.List.Extra
import Test.Hspec

import Hhp

spec :: Spec
spec = do
    describe "lintSyntax" $ do
        it "check syntax with HLint" $ do
            res <- lintSyntax defaultOptions "test/data/hlint.hs"
            -- hlint displays additional line infomation
            -- since unknown version (:5:16-26:)
            replace "\\" "/" (replace "16-26" "16" res) `shouldBe` "test/data/hlint.hs:5:16: Warning: Use sum\NULFound:\NUL  foldr (+) 0\NULPerhaps:\NUL  sum\n"

        context "without suggestions" $ do
            it "doesn't output empty line" $ do
                res <- lintSyntax defaultOptions "test/data/hhp-check/Data/Foo.hs"
                res `shouldBe` ""
#else
import Test.Hspec

spec :: Spec
spec = do
    describe "lintSyntax" $ do
        it "check syntax with HLint" $ do True `shouldBe` True
#endif
