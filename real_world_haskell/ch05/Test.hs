-- This is created when reading Ch11
import Test.QuickCheck
import Prettify
import Control.Monad (liftM, liftM2)

-- This is a better version without intermediate vars.
instance Arbitrary Doc where
        arbitrary =
            oneof [ return Empty
                  , liftM Char arbitrary
                  , liftM Text arbitrary
                  , return Line
                  , liftM2 Concat arbitrary arbitrary
                  , liftM2 Union arbitrary arbitrary]

prop_empty_id x = 
    empty <> x == x
  &&
    x <> empty == x

prop_char c = char c == Char c
prop_text s = text s == if null s then Empty else Text s
prop_line = line == Line
prop_double d = double d == text (show d)

main = do
    verboseCheck ((\_ -> True) :: Doc -> Bool)
    quickCheck prop_empty_id
    quickCheck prop_char
    quickCheck prop_text
    quickCheck prop_line
    quickCheck prop_double
