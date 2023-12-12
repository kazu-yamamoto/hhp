import Dir
import Spec

import System.Process
import Test.Hspec

main :: IO ()
main = do
    let sandboxes = ["test/data", "test/data/check-packageid"]
        genSandboxCfg dir = withDirectory dir $ \cwdir -> do
            system
                ( "sed 's|@CWD@|" ++ cwdir ++ "|g' cabal.sandbox.config.in > cabal.sandbox.config"
                )
    genSandboxCfg `mapM_` sandboxes
    hspec spec
