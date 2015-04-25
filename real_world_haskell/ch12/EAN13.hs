-- Refer http://en.wikipedia.org/wiki/International_Article_Number_%28EAN%29
-- For details of EAN-13 barcode.
module EAN13 (encodeDigits) where

import Data.List (foldl', foldl1')
import Data.Array (Ix, Array(..), listArray, indices, (!), bounds, elems)
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

outerGuard = "101"
centerGuard = "01010"

-- -------------------------------------
-- foldA version from the book page 273
-- -------------------------------------
-- | Strict left fold, similar to foldl' on lists
-- Use elems of Array module is better.
{-
foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f z a = go z (indices a)
    where go z (j:js) = let z' = f z (a ! j)
                        in z' `seq` go z' js
          go z _ = z
-}

-- | Strict left fold using the first element of the array as its
-- starting value, similar to foldl1 on lists
--
-- This version has a bug that the 1st number would be the initial value,
-- and it will also be traversed another time.
{-
foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f a = foldA f (a ! fst (bounds a)) a
-}
                            

-- -------------------------------------------------------------------
-- foldA version by me, comments from the on line comments referenced
-- -------------------------------------------------------------------

-- | Strict left fold, similar to foldl' on lists
foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f z a = foldl' f z $ elems a

-- | Strict left fold using the first element of the array as its
-- starting value, similar to foldl1 on lists
foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f a = foldl1' f $ elems a


-- | This function computes the barcode with given 12 digits,
-- the last checksum is computed by it.
encodeDigits :: [Int] -> [String]
encodeDigits s@(first:rest) =
    outerGuard : lefties ++ centerGuard : righties ++ [outerGuard]
    -- The book 'splitAt 5' has a bug, it should be '6' as below.
    where (left, right) = splitAt 6 rest
          lefties = zipWith leftEncode (leftParityCodes ! first) left
          righties = map rightEncode (right ++ [checkDigit s])

leftEncode :: Char -> Int -> String
leftEncode '1' = (leftOddCodes !)
leftEncode '0' = (leftEvenCodes !)

rightEncode :: Int -> String
rightEncode = (rightEvenCodes !)

