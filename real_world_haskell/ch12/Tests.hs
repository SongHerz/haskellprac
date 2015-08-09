module Tests where

import Test.QuickCheck
import EAN13
import Data.Array (array)
import Data.Char (digitToInt)

-- Convert a digit char to RGB
bitCharToRGB :: Char -> RGB
bitCharToRGB '0' = (255, 255, 255)
bitCharToRGB '1' = (0, 0, 0)
bitCharToRGB x   = error $ "Unsupported character" ++ [x]

-- Give 12 digits, and return a Pixmap and the checksum digit
encodeDigitsToPixmap :: [Int] -> (Pixmap, Int)
encodeDigitsToPixmap xs = (pm, cs)
    where (digits, cs) = encodeDigits xs
          char_series = "00" ++ (concat digits) ++ "00"
          row = map bitCharToRGB char_series
          w = length row
          h = w
          pm = array ((0, 0), (w - 1, h - 1)) [((x, y), p) | (x, p) <- zip [0..] row, y <- [0..(h - 1)]]

prop_closeloop :: Bool
prop_closeloop = let digits = [1,2,3,4,5,6,7,8,9,0,1,2]
                     (pm, cs) = encodeDigitsToPixmap digits
                 in case findEAN13 pm of
                        Just xs -> xs == map fromIntegral (digits ++ [cs])
                        Nothing -> False
