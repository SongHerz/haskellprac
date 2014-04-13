import Data.List (isPrefixOf)
import Data.Char (isSpace)

class BasicEq a where
    isEqual :: a -> a -> Bool

instance BasicEq Bool where
    isEqual True  True  = True
    isEqual False False = True
    isEqual _     _     = False


class BasicEq2 a where
    isEqual2    :: a -> a -> Bool
    isNotEqual2 :: a -> a -> Bool


class BasicEq3 a where
    isEqual3 :: a -> a -> Bool
    isEqual3 x y = not (isNotEqual3 x y)

    isNotEqual3 :: a -> a -> Bool
    isNotEqual3 x y = not (isEqual3 x y)

-- When no isEqual3 or isNotEqual3 defined for a type instance,
-- with default implementation from the type class,
-- (isEqual3/isNotEqual3 Bool Bool) would run recursively endlessly
-- until stack overflow.
instance BasicEq3 Bool


data Color = Red | Green | Blue
instance BasicEq3 Color where
    isEqual3 Red Red = True
    isEqual3 Green Green = True
    isEqual3 Blue Blue = True
    isEqual3 _ _ = False

instance Show Color where
    show Red   = "Red"
    show Green = "Green"
    show Blue  = "Blue"

-- The is the implementation from page 142, but (isPrefixOf) is used
-- instead of (take)
-- instance Read Color where
--     -- readsPrec is the main function for parsing input
--     readsPrec _ value =
--         -- We pass tryParse a list of pairs. Each pair has a string
--         -- and the desired return value. tryParse will try to match
--         -- the input to one of these strings.
--         tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
--         where tryParse [] = []  -- If there is nothing left to try, fail
--               tryParse ((attempt, result):xs) =
--                       -- Compare the start of the string to be parsed to the
--                       -- text we are looking for.
--                       if attempt `isPrefixOf` value
--                          -- If we have a match, return the result and the
--                          -- remaining input
--                          then [(result, drop (length attempt) value)]
--                          -- If we don't have a match, try the next pair
--                          -- in the list of attempts.
--                          else tryParse xs

-- This is the enhanced version with dropWhile and preceding spaces trimmed
-- At least, I think this version is much shorter and cleaner than the one
-- given on the book.
instance Read Color where
    readsPrec _ value =
        case dropWhile (not.(`isPrefixOf` trimmedValue).fst) attemptResultPairs of
            (attempt, result) : _ -> [(result, drop (length attempt) trimmedValue)]
            _                     -> []
        where trimmedValue = dropWhile isSpace value
              attemptResultPairs = [("Red", Red), ("Green", Green), ("Blue", Blue)]
