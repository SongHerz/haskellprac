import Data.Char (digitToInt, isDigit)
import Data.List (foldl')

loop :: Int -> String -> Int
loop acc []     = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs


asInt :: String -> Int
asInt xs = loop 0 xs

--
-- For ex1 ~ ex4 on page 97 - 98
--
--
-- foldl version is OK, 
-- but probably we can use foldl' to save thunk memory,
-- as converting a string to an integer requires going through the whole string.
asInt_foldl :: String -> Int
asInt_foldl xs = foldl step 0 xs
    where step acc x = acc * 10 + digitToInt x

asInt_foldl' :: String -> Int
asInt_foldl' [] = error "String is empty"
asInt_foldl' xs
    | head xs == '-' = negate (inner (tail xs))
    | otherwise      = inner xs
    where inner [] = error "No digit found"
          inner xs = foldl' step 0 xs
          step acc x
            | isDigit x = let newDigit = digitToInt x
                              newAcc = acc * 10 + newDigit
                          in if (newAcc - newDigit) `div` 10 == acc
                             then newAcc
                             else error "overflow"
            | otherwise = error ('\'': x : "' is not a digit")


-- Obviously, foldl version is much cleaner better
asInt_foldr :: String -> Int
asInt_foldr xs = snd (foldr step (1,0) xs)
    where step x (amp, res) = (amp * 10, digitToInt x * amp + res)
