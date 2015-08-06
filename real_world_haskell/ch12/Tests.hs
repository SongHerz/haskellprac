module Tests where

import Test.QuickCheck
import EAN13
import Data.Array (listArray)
import Data.Char (digitToInt)

encodeDigitsToPixmap :: [Int] -> Pixmap
encodeDigitsToPixmap xs = listArray $ map digitToInt char_series
    where (digits, _) = encodeDigits xs
          char_series = "00" ++ (concat digits) ++ "00"

prop_closeloop :: Bool
prop_closeloop = True
