{-# LANGUAGE FlexibleInstances #-}
-- Need this, because Data.Either has already defined 
-- 'instance Functor (Either a)'
{-# LANGUAGE OverlappingInstances #-}

instance Functor (Either Int) where
    fmap _ (Left n)     = Left n
    fmap f (Right v)    = Right $ f v
