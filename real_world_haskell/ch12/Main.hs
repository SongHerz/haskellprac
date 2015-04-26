module Main where

import System.Environment (getArgs)
import Data.List (groupBy)
import qualified Encoder

main = do
    args <- getArgs
    line <- getLine
    showEncoding (not.null $ args) $ line2Digits line
    where showEncoding sel = if sel then showGUIEncoding else showTextEncoding 
          line2Digits :: String -> [Int]
          line2Digits line = map read $ groupBy (\_ _ -> False) line

showTextEncoding :: [Int] -> IO ()
showTextEncoding xs = putStrLn $ Encoder.textEncode xs

showGUIEncoding :: [Int] -> IO ()
-- As gloss will quit the whole app when its window closed.
-- And I have no idea why.
-- There is a stackoverflow thread discussing the problem:
-- http://stackoverflow.com/questions/29381466/how-to-get-gloss-to-not-close-ghci
-- but I have not tried the solution yet, and from FAQ referenced in the
-- thread, it seems the answer may not solve the problem.
--
-- So NOTHING SHOULD BE DONE AFTER showing the gloss window.
showGUIEncoding xs = Encoder.guiEncode xs
