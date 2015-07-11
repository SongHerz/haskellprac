module Main where

import System.Environment (getArgs)
import Data.List (groupBy)
import qualified Encoder
import Options.Applicative

data Config = Config {
      _12Digits :: String
    , gui :: Bool
    }

config :: Parser Config
config = Config
    <$> strOption (
           short 'd'
        <> metavar "12DIGITS"
        <> help "12 digits for EAN13 coding")
    <*> switch (
           long "gui"
        <> help "Show in GUI, when enabled")

opts = info (helper <*> config) (
               fullDesc
            <> progDesc "Encode 12 digits to EAN13 bar code"
            <> header "EAN13 encoder")


showEncoding :: Config -> IO ()
showEncoding (Config xs gui) =
    case gui of
        False -> showTextEncoding digits
        True  -> showGUIEncoding digits
    where digits = map read $ groupBy (\_ _ -> False) xs

main = execParser opts >>= showEncoding

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
