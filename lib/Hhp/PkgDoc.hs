module Hhp.PkgDoc (packageDoc) where

import System.Process (readProcess)

import Hhp.GhcPkg
import Hhp.Types

-- | Obtaining the package name and the doc path of a module.
packageDoc
    :: Options
    -> Cradle
    -> ModuleString
    -> IO String
packageDoc _ cradle mdl = pkgDoc cradle mdl

pkgDoc :: Cradle -> String -> IO String
pkgDoc cradle mdl = do
    pkg <- trim <$> readProcess "ghc-pkg" toModuleOpts []
    if pkg == ""
        then return "\n"
        else do
            htmlpath <- readProcess "ghc-pkg" (toDocDirOpts pkg) []
            let ret = pkg ++ " " ++ drop 14 htmlpath
            return ret
  where
    toModuleOpts =
        ["find-module", mdl, "--simple-output"]
            ++ ghcPkgDbStackOpts (cradlePkgDbStack cradle)
    toDocDirOpts pkg =
        ["field", pkg, "haddock-html"]
            ++ ghcPkgDbStackOpts (cradlePkgDbStack cradle)
    trim = takeWhile (`notElem` " \n")
