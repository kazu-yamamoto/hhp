module CheckSpec where

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import System.FilePath
import Test.Hspec

import Hhp
import Hhp.Cradle

import Dir

spec :: Spec
spec = do
    describe "checkSyntax" $ do
        it "can check even if an executable depends on its library" $ do
            withDirectory_ "test/data/hhp-check" $ do
                cradle <- findCradleWithoutSandbox
                res <- checkSyntax defaultOptions cradle ["main.hs"]
                res
                    `shouldBe` "main.hs:5:1:Warning: Top-level binding with no type signature: main :: IO ()\n"

        it
            "can check even if a test module imports another test module located at different directory" $ do
            withDirectory_ "test/data/check-test-subdir" $ do
                cradle <- findCradleWithoutSandbox
                res <- checkSyntax defaultOptions cradle ["test/Bar/Baz.hs"]
                res
                    `shouldSatisfy` ( ( "test"
                                            </> "Foo.hs:3:1:Warning: Top-level binding with no type signature: foo :: String\n"
                                      )
                                        `isSuffixOf`
                                    )

        it "can detect mutually imported modules" $ do
            withDirectory_ "test/data" $ do
                cradle <- findCradleWithoutSandbox
                res <- checkSyntax defaultOptions cradle ["Mutual1.hs"]
                res `shouldSatisfy` ("a cycle" `isInfixOf`)

        it "can check a module using QuasiQuotes" $ do
            withDirectory_ "test/data" $ do
                cradle <- findCradleWithoutSandbox
                res <- checkSyntax defaultOptions cradle ["Baz.hs"]
                res `shouldSatisfy` ("Baz.hs:7:1:Warning:" `isPrefixOf`)

        context "without errors" $ do
            it "doesn't output empty line" $ do
                withDirectory_ "test/data/hhp-check/Data" $ do
                    cradle <- findCradleWithoutSandbox
                    res <- checkSyntax defaultOptions cradle ["Foo.hs"]
                    res `shouldBe` ""
