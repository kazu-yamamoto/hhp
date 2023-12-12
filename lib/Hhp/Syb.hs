{-# LANGUAGE RankNTypes #-}

module Hhp.Syb (
    listifySpans,
) where

import GHC (GenLocated (L), TypecheckedSource, isGoodSrcSpan, spans)

import Data.Generics (GenericQ, Typeable, gmapQ, mkQ)

import Hhp.Gap

listifySpans :: Typeable a => TypecheckedSource -> (Int, Int) -> [LOC a]
listifySpans tcs lc = everything' (++) ([] `mkQ` (\x -> [x | p x])) tcs
  where
    p (L spn _) = isGoodSrcSpan (locA spn) && (locA spn) `spans` lc

everything' :: (r -> r -> r) -> GenericQ r -> GenericQ r
everything' k f x = foldl k (f x) $ gmapQ (everything' k f) x
