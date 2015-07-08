
module Encoder (textEncode, guiEncode) where

import qualified EAN13

import Data.List (intercalate)
import Data.Char (intToDigit)
import qualified Graphics.Gloss as G


internalEncode :: [Int] -> Either String ([String], Int)
internalEncode xs
    | length xs == 12 = Right $ EAN13.encodeDigits xs
    | otherwise       = Left $ "Code length should be 12, but given code length is " ++ show (length xs)

-- | Convert EAN13 code in [String] presentation to human readable form.
toHumanReadable :: [String] -> String
toHumanReadable = intercalate ", "


textEncode :: [Int] -> String
textEncode xs = case internalEncode xs of
                    Right (ys, _) -> toHumanReadable ys
                    Left err -> err

guiEncode :: [Int] -> IO ()
guiEncode xs = do 
    case internalEncode xs of
        Right (ys, cs) -> do 
                       putStrLn $ toHumanReadable ys
                       guiShow xs ys cs
        Left err -> putStrLn err

-- | Show the EAN13 barcode in a window.
-- xs: 12 digits list
-- ys: '0/1' string list including guards.
-- cs: checksum of xs
guiShow :: [Int] ->[String] -> Int -> IO ()
guiShow xs ys cs = do
    G.display (G.InWindow "EAN13" (ceiling w + 20, ceiling h + 20) (10, 10)) G.white pic
    where (pic, w, h) = drawCode xs cs ys

-- | Draw picture of a EAN13 barcode.
-- xs: 12 digits list.
-- cs: the checksum digit.
-- ys: The string list that contains digits and guards.
-- Return the picture, and width and height of it.
drawCode :: [Int] -> Int -> [String] -> (G.Picture, Float, Float)
drawCode xs cs ys = (pic, totalWidth, guardH)
    where pic = G.translate (- totalWidth / 2) 0 $ G.pictures $ concat [
                drawBitsDigitSeries [(leftMostBits, leftMostDigit)] w bitH digitW digitH leftMostBitsOffset bitYO
              , drawBits leftGuard w guardH leftGuardOffset guardYO
              , drawBitsDigitSeries (zip leftBits leftDigits) w bitH digitW digitH leftBitsOffset bitYO
              , drawBits innerGuard w guardH innerGuardOffset guardYO
              , drawBitsDigitSeries (zip rightBits rightDigits) w bitH digitW digitH rightBitsOffset bitYO
              , drawBits rightGuard w guardH rightGuardOffset guardYO
              ]

          w = 2
          bitH = 120
          bitYO = (guardH - bitH) / 2
          guardH = bitH * 1.2
          guardYO = 0
          digitH = guardH - bitH
          digitW = w * 7.0

          leftMostBits = "0000000"
          (leftGuard, leftBits, innerGuard, rightBits, rightGuard) = toBits ys
          (leftMostDigit, leftDigits, rightDigits) = toDigits xs cs

          leftMostBitsOffset = 0.0
          leftGuardOffset = leftMostBitsOffset + w * fromIntegral (length leftMostBits)
          leftBitsOffset = leftGuardOffset + w * fromIntegral (length leftGuard)
          innerGuardOffset = leftBitsOffset + w * fromIntegral (sum $ map length leftBits)
          rightBitsOffset = innerGuardOffset + w * fromIntegral (length innerGuard)
          rightGuardOffset = rightBitsOffset + w * fromIntegral (sum $ map length rightBits)
          totalWidth = rightGuardOffset + w * fromIntegral (length rightGuard)

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

-- | Draw bits with a digit.
-- With given '0'/'1' string
-- bit width: bw
-- bit height: bh
-- d: a digit
-- digit width: dw
-- digit height: dh
-- x offset: xo
-- y offset: yo
drawBitsDigit :: String -> Float -> Float -> Int -> Float -> Float -> Float -> Float -> [G.Picture]
drawBitsDigit xs bw bh d dw dh xo yo = digitPic : drawBits xs bw bh xo yo
    where fontDefaultWidth = 100.0
          fontDefaultHeight = 100.0
          fontZoomRatioW = dw / fontDefaultWidth
          fontZoomRatioH = dh / fontDefaultHeight
          digitPic = G.translate xo (yo - bh / 2 - dh) $ G.scale fontZoomRatioW fontZoomRatioH $ G.text [intToDigit d]

-- | Draw bits digit pairs
-- bit width: bw
-- bit height: bh
-- digit width: dw
-- digit height: dh
-- x offset: xo
-- y offset: yo
drawBitsDigitSeries :: [(String, Int)] -> Float -> Float -> Float -> Float -> Float -> Float -> [G.Picture]
drawBitsDigitSeries [] _ _ _ _ _ _ = []
drawBitsDigitSeries ((xs, d) : rest) bw bh dw dh xo yo =
    drawBitsDigit xs bw bh d dw dh xo yo ++ others 
    where others = drawBitsDigitSeries rest bw bh dw dh (xo + (bw * (fromIntegral $ length xs))) yo

-- | Split bits
-- Split a given EAN13 bar code to
-- (leftGuard, leftBits, innerGuard, rightBits, rightGuard)
toBits :: [String] -> (String, [String], String, [String], String)
toBits xs = map5Tuple concat (leftGuard, leftBits, innerGuard, rightBits, rightGuard)
    where (leftGuard, rest0) = splitAt 1 xs
          (leftBits, rest1) = splitAt 6 rest0
          (innerGuard, rest2) = splitAt 1 rest1
          (rightBits, rightGuard) = splitAt 6 rest2
          map5Tuple f (a, b, c, d, e) = (f a, b, f c, d, f e)

-- | Split digits
-- Split given code and checksum to
-- (leftMostDigit, leftDigits, rightDigits)
-- _12Digits: 12 digits list 
-- cs: the checksum digit
toDigits :: [Int] -> Int -> (Int, [Int], [Int])
toDigits _12Digits cs= (leftMostDigit, leftDigits, rightDigits)
    where (leftMostDigit : _, rest0) = splitAt 1 _12Digits
          (leftDigits, rest1) = splitAt 6 rest0
          rightDigits = rest1 ++ [cs]
