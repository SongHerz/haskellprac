
module Encoder (textEncode) where

import qualified EAN13
import Data.List (intercalate)


internalEncode :: [Int] -> Either String [String]
internalEncode xs
    | length xs == 12 = Right $ EAN13.encodeDigits xs
    | otherwise       = Left $ "Code length should be 12, but given code length is " ++ show (length xs)


textEncode :: [Int] -> String
textEncode xs = case internalEncode xs of
                    Right ys -> intercalate ", " ys
                    Left err -> err
