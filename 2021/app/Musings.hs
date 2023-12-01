module Musings where

import Data.Monoid


data Foo a b = Foo a b
  deriving (Show)



type ProvenanceT a b x = (First a, b x)

whomst :: m a -> b -> ProvenanceT a b
whomst a b = (First $ Just a, b)

whomstdve :: Provenance a b -> Maybe a
whomstdve (First p, _) = p




  
