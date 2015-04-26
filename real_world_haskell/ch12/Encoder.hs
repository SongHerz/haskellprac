
module Encoder (textEncode, guiEncode) where

import qualified EAN13

import Data.List (intercalate)
import qualified Graphics.Gloss as G


internalEncode :: [Int] -> Either String [String]
internalEncode xs
    | length xs == 12 = Right $ EAN13.encodeDigits xs
    | otherwise       = Left $ "Code length should be 12, but given code length is " ++ show (length xs)

-- | Convert EAN13 code in [String] presentation to human readable form.
toHumanReadable :: [String] -> String
toHumanReadable = intercalate ", "


textEncode :: [Int] -> String
textEncode xs = case internalEncode xs of
                    Right ys -> toHumanReadable ys
                    Left err -> err

guiEncode :: [Int] -> IO ()
guiEncode xs = do 
    case internalEncode xs of
        Right ys -> do 
                       putStrLn $ toHumanReadable ys
                       guiShow ys
        Left err -> putStrLn err

guiShow :: [String] -> IO ()
guiShow _ = do
    G.display (G.InWindow "EAN13" (100, 100) (10, 10)) G.white G.blank
