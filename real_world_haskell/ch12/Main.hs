module Main where

import System.Environment (getArgs)
import Data.List (groupBy)
import qualified Encoder
import qualified Decoder
import Options.Applicative

data Action =
      Encode { _12Digits :: String }
    | Decode { picPath :: FilePath }

data Config = Config {
      action :: Action
    , gui :: Bool
    }

parseAction :: Parser Action
parseAction = (Encode
              <$> strOption (
                    short 'e'
                 <> long "encode"
                 <> metavar "12_DIGITS"
                 <> help "12 digits for EAN13 coding"))
        <|> (Decode
             <$> strOption (
                       short 'd'
                    <> long "decode"
                    <> metavar "PIC_PATH"
                    <> help "Path to a picture that conains EAN13 bar code"))

parseConfig :: Parser Config
parseConfig = Config
    <$> parseAction
    <*> switch (
           long "gui"
        <> help "Show in GUI, when enabled")

opts = info (helper <*> parseConfig) (
               fullDesc
            <> progDesc "Encode 12 digits to EAN13 bar code, or decode EAN13 bar code."
            <> header "EAN13 encoder/decoder")


showEncoding :: String -> Bool -> IO ()
showEncoding xs gui =
    case gui of
        False -> showTextEncoding digits
        True  -> showGUIEncoding digits
    where digits = map read $ groupBy (\_ _ -> False) xs

showDecoding :: FilePath -> Bool -> IO ()
showDecoding path gui = Decoder.decodePic path

run :: Config -> IO ()
run (Config act gui) =
    case act of
        Encode _12Digits -> showEncoding _12Digits gui
        Decode path -> showDecoding path gui

main = execParser opts >>= run

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
