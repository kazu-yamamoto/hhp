module Hhp.Lint where

import Control.Exception (handle, SomeException(..))
import Language.Haskell.HLint (hlint)

import Hhp.Logger (checkErrorPrefix)
import Hhp.Types

-- | Checking syntax of a target file using hlint.
--   Warnings and errors are returned.
lintSyntax :: Options
           -> FilePath  -- ^ A target file.
           -> IO String
lintSyntax opt file = handle handler $ pack <$> hlint (file : "--quiet" : hopts)
  where
    pack = convert opt . map (init . show) -- init drops the last \n.
    hopts = hlintOpts opt
    handler (SomeException e) = return $ checkErrorPrefix ++ show e ++ "\n"
