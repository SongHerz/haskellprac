-- Refer http://en.wikipedia.org/wiki/International_Article_Number_%28EAN%29
-- For details of EAN-13 barcode.
module EAN13 where

-- http://www.gs1.org/check-digit-calculator
-- 978354004934(0)
-- Not the same as the book.
-- The version in the book has a bug, check sum 0 will be outputed as 10.
checkDigit :: (Integral a) => [a] -> a
checkDigit ds = (10 - sum products) `mod` 10
    where products = mapEveryOther (*3) (reverse ds)

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f, id])
