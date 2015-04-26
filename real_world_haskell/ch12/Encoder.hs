
module Encoder (textEncode, guiEncode) where

import qualified EAN13
import Data.List (intercalate)


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

guiEncode :: [Int] -> IO String
guiEncode xs = do 
    case internalEncode xs of
        Right ys -> do 
                       guiShow ys
                       return $ toHumanReadable ys
        Left err -> return err

guiShow :: [String] -> IO ()
guiShow _ = return ()
