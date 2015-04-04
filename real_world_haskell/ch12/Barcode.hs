-- Refer http://en.wikipedia.org/wiki/International_Article_Number_%28EAN%29
-- For details of EAN-13 barcode.
module EAN13 where

import Data.Array (Array(..), listArray)
import Control.Applicative ((<$>))

-- http://www.gs1.org/check-digit-calculator
-- 978354004934(0)
-- Not the same as the book.
-- The version in the book has a bug, check sum 0 will be outputed as 10.
checkDigit :: (Integral a) => [a] -> a
checkDigit ds = (10 - sum products) `mod` 10
    where products = mapEveryOther (*3) (reverse ds)

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f, id])

-- Encoding of digits
leftOddList = [ "0001101"   -- 0
              , "0011001"   -- 1
              , "0010011"   -- 2
              , "0111101"   -- 3
              , "0100011"   -- 4
              , "0110001"   -- 5
              , "0101111"   -- 6
              , "0111011"   -- 7
              , "0110111"   -- 8
              , "0001011"   -- 9
              ]

rightEvenList = map complement <$> leftOddList
    where complement '0' = '1'
          complement '1' = '0'

leftEvenList = map reverse rightEvenList

-- The parity list is for left 6 digits only.
-- And the parity combinations of the left 6 digits determine the 1st digit
-- of the bar code.
-- '0' even parity.
-- '1' odd parity.
-- 
-- In the wikipedia mentioned at the beginning of this module,
-- This table is called "Structure of EAN-13"
--
--                 Parity      1st Digit
leftParityList = [ "111111"     -- 0
                 , "110100"     -- 1
                 , "110010"     -- 2
                 , "110001"     -- 3
                 , "101100"     -- 4
                 , "100110"     -- 5
                 , "100011"     -- 6
                 , "101010"     -- 7
                 , "101001"     -- 8
                 , "100101"     -- 9
                 ]

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0, l - 1) xs
    where l = length xs

leftOddCodes, leftEvenCodes, rightEvenCodes, leftParityCodes :: Array Int String

leftOddCodes = listToArray leftOddList
leftEvenCodes = listToArray leftEvenList
rightEvenCodes = listToArray rightEvenList
leftParityCodes = listToArray leftParityList
