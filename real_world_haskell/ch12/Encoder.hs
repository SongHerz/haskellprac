
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
guiShow xs = do
    G.display (G.InWindow "EAN13" (600, 200) (10, 10)) G.white (drawCode xs)

drawCode :: [String] -> G.Picture
drawCode xs = G.pictures $ concat [
                drawBits leftGuard w guardH leftGuardOffset guardYO
              , drawBits leftBits w bitH leftBitsOffset bitYO
              , drawBits innerGuard w guardH innerGuardOffset guardYO
              , drawBits rightBits w bitH rightBitsOffset bitYO
              , drawBits rightGuard w guardH rightGuardOffset guardYO
              ]
    where w = 2
          bitH = 120
          bitYO = 0
          guardH = bitH * 1.2
          guardYO = - (guardH - bitH) / 2

          (leftGuard, leftBits, innerGuard, rightBits, rightGuard) = toBits xs
          leftGuardOffset = 0.0
          leftBitsOffset = leftGuardOffset + w * fromIntegral (length leftGuard)
          innerGuardOffset = leftBitsOffset + w * fromIntegral (length leftBits)
          rightBitsOffset = innerGuardOffset + w * fromIntegral (length innerGuard)
          rightGuardOffset = rightBitsOffset + w * fromIntegral (length rightBits)

-- | Draw a bit.
-- Char c: '0'/'1'
-- width: w
-- height: h
-- x offset: xo
-- y offset: yo
drawBit :: Char -> Float -> Float -> Float -> Float -> G.Picture
drawBit c w h xo yo = G.color color $ G.translate xo yo $ G.rectangleSolid w h
                where color = if c == '0'
                              then G.white
                              else G.black

-- | Draw bits.
-- With given '0'/'1' string
-- width: w
-- height: h
-- x offset: xo
-- y offset: yo
drawBits :: String -> Float -> Float -> Float -> Float -> [G.Picture]
drawBits [] _ _ _ _ = []
drawBits (c:xs) w h xo yo = drawBit c w h xo yo : drawBits xs w h (xo + w) yo

-- | Split bits
-- Split a given EAN13 bar code to
-- (leftGuard, leftBits, innerGuard, rightBits, rightGuard)
toBits :: [String] -> (String, String, String, String, String)
toBits xs = map5Tuple concat (leftGuard, leftBits, innerGuard, rightBits, rightGuard)
    where (leftGuard, rest0) = splitAt 1 xs
          (leftBits, rest1) = splitAt 6 rest0
          (innerGuard, rest2) = splitAt 1 rest1
          (rightBits, rightGuard) = splitAt 6 rest2
          map5Tuple f (a, b, c, d, e) = (f a, f b, f c, f d, f e)
