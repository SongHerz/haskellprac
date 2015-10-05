{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

data Zero
data Succ n

type Three = Succ (Succ (Succ Zero))
u = undefined :: Three

data Nil = Nil
data Cons x xs = Cons
type Singleton x = Cons x Nil

-- Generate descending sequence of numbers
class DownFrom n xs | n -> xs where downfrom :: n -> xs
instance DownFrom Zero Nil
instance DownFrom n xs => DownFrom (Succ n) (Cons n xs)

-- Compare numbers
data T
data F
class Lte a b c | a b -> c where lte :: a -> b -> c
instance Lte Zero b T
instance Lte (Succ n) Zero F
instance Lte a b c => Lte (Succ a) (Succ b) c

-- Insertion sort
class Insert x xs ys | x xs -> ys where insert :: x -> xs -> ys
instance Insert x Nil (Singleton x)
instance (Lte x y b, InsertCons b x y ys) => Insert x (Cons y ys) r

class InsertCons b x1 x2 xs ys | b x1 x2 xs -> ys
instance InsertCons T x1 x2 xs (Cons x1 (Cons x2 xs))
instance Insert x1 xs ys => InsertCons F x1 x2 xs (Cons x2 ys)

class Sort xs ys | xs -> ys where sort :: xs -> ys
instance Sort Nil Nil
instance (Sort xs ys, Insert x ys zs) => Sort (Cons x xs) zs
