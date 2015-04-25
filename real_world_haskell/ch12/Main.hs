module Main where

import qualified Encoder

main = do
    line <- getLine
    putStrLn $ Encoder.textEncode $ line2Digits line

line2Digits :: String -> [Int]
line2Digits line = map read $ words line
