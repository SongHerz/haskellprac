module Tests where

import Test.QuickCheck
import EAN13
import Data.Array (listArray)
import Data.Char (digitToInt)

-- Convert a digit char to RGB
bitCharToRGB :: Char -> RGB
bitCharToRGB '0' = (255, 255, 255)
bitCharToRGB '1' = (0, 0, 0)
bitCharToRGB x   = error $ "Unsupported character" ++ [x]

encodeDigitsToPixmap :: [Int] -> Pixmap
encodeDigitsToPixmap xs = listArray ((0, 0), (w - 1, h - 1)) $ concat $ take h $ repeat row
    where (digits, _) = encodeDigits xs
          char_series = "00" ++ (concat digits) ++ "00"
          row = map bitCharToRGB char_series
          w = length row
          h = w

prop_closeloop :: Bool
prop_closeloop = True
