module DList where

import Data.Function ((.), id)
import Data.Monoid (Monoid (mempty))
import Data.Semigroup (Semigroup ((<>)))

newtype DList a = DList ([a] -> [a])

toList :: DList a -> [a]
toList (DList f) = f []

singleton :: a -> DList a
singleton x = DList (x :)

instance Semigroup (DList a)
  where
    DList f <> DList g = DList (f . g)

instance Monoid (DList a)
  where
    mempty = DList id
