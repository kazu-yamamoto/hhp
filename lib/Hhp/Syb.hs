{-# LANGUAGE RankNTypes #-}

module Hhp.Syb (
    listifySpans
  ) where

import GHC (TypecheckedSource, Located, GenLocated(L), isGoodSrcSpan, spans)

import Data.Generics

listifySpans :: Typeable a => TypecheckedSource -> (Int, Int) -> [Located a]
listifySpans tcs lc = everything' (++) ([] `mkQ` (\x -> [x | p x])) tcs
  where
    p (L spn _) = isGoodSrcSpan spn && spn `spans` lc

everything' :: (r -> r -> r) -> GenericQ r -> GenericQ r
everything' k f x = foldl k (f x) $ gmapQ (everything' k f) x
