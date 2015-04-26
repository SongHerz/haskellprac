module Main where

import System.Environment (getArgs)
import qualified Encoder

main = do
    args <- getArgs
    line <- getLine
    showEncoding (not.null $ args) $ line2Digits line
    where showEncoding sel = if sel then showGUIEncoding else showTextEncoding 
          line2Digits :: String -> [Int]
          line2Digits line = map read $ words line

showTextEncoding :: [Int] -> IO ()
showTextEncoding xs = putStrLn $ Encoder.textEncode xs

showGUIEncoding :: [Int] -> IO ()
showGUIEncoding xs = do
    msg <- Encoder.guiEncode xs
    putStrLn msg
