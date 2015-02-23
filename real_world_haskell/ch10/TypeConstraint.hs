-- Page 247
-- Constrains on type definitions are BAD !!!

-- Need to enable this extension to add constrain on type constrain.
{-# LANGUAGE DatatypeContexts #-}

{-
- I got warning message from ghci 7.6.3
-
    -XDatatypeContexts is deprecated: It was widely considered a misfeature, and has been removed from the Haskell language.
  
  And I think the feature is going to be removed in some later release,
  though it currently works for 7.6.3 verion.
-}

data (Ord a) => OrdStack a = Bottom
                           | Item a (OrdStack a)
                           deriving (Show)

isIncreasing :: (Ord a) => OrdStack a -> Bool
isIncreasing (Item a rest@(Item b _))
    | a < b     = isIncreasing rest
    | otherwise = False
isIncreasing _  = True


-- Need "Ord a" constrain
push :: (Ord a) => a -> OrdStack a -> OrdStack a
push a s = Item a s
