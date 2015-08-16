module DList (
    DList
  , fromList
  , toList
  , empty
  , append
  , cons
  , dfoldr
  , safeHead) where

import Data.Monoid

newtype DList a = DL {
    unDL :: [a] -> [a]
    }

fromList :: [a] -> DList a
fromList xs = DL (xs ++)

toList :: DList a -> [a]
toList (DL xs) = xs []

empty :: DList a
empty = DL id

append :: DList a -> DList a -> DList a
append (DL xs) (DL ys) = DL (xs . ys)

cons :: a -> DList a -> DList a
x `cons` (DL xs) = DL ((x:) . xs)
infixr `cons`

dfoldr :: (a -> b -> b) -> b -> DList a -> b
dfoldr f acc ds = foldr f acc $ toList ds

safeHead :: DList a -> Maybe a
safeHead xs = case toList xs of
                  (y:_) -> Just y
                  _ -> Nothing

dmap :: (a -> b) -> DList a -> DList b
dmap f = dfoldr go empty
    where go x xs = (f x) `cons` xs

instance Functor DList where
    fmap = dmap

instance Monoid (DList a) where
    mempty = empty
    mappend = append
