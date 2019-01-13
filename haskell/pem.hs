{-# LANGUAGE GADTs, RankNTypes #-}

module Kata where

import Data.Void

type AxiomPEM = forall a. forall b. (a -> b) -> ((a -> Void) -> b) -> b

-- --p -> p
type AxiomDNE = forall a. ((a -> Void) -> Void) -> a


from :: AxiomDNE -> AxiomPEM
from dnee ab avb = dnee avv
  where
    -- avv :: (b -> Void) -> Void
    -- (b -> Void, b) -> Void
    avv bv = bv b
      where
        -- av :: a -> Void
        av a = bv $ ab a
        b = avb av



to :: AxiomPEM -> AxiomDNE
to pem nna = pem id (absurd . nna)
