
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

data Bar = PureBar String
         | DigitBar String Int

data BarCode = BarCode {
      height            :: Float
    , bitWidth          :: Float
    , digitHeightRatio  :: Float
    , bars              :: [Bar]
    }

-- | Convert digits and strings to BarCode
-- xs : 12 digits list
-- ys: '0/1' stringlist including guards
-- cs: checksum of xs
toBarCode :: [Int] -> [String] -> Int -> Float -> Float -> Float -> BarCode
toBarCode xs ys cs h bw dhr = BarCode {
      height = h
    , bitWidth = bw
    , digitHeightRatio = dhr
    , bars = concat [
              [DigitBar leftMostBits leftMostDigit]
            , [PureBar leftGuard]
            , zipWith DigitBar leftBits leftDigits
            , [PureBar innerGuard]
            , zipWith DigitBar rightBits rightDigits
            , [PureBar rightGuard]]
            }
    where leftMostBits = "0000000"
          (leftGuard, leftBits, innerGuard, rightBits, rightGuard) = toBits ys
          (leftMostDigit, leftDigits, rightDigits) =  toDigits xs cs

-- | Draw BarCode as a picture
-- Return (width, height, Picture)
drawBarCode :: BarCode -> (Float, Float, G.Picture)
drawBarCode bc = (width, height bc, G.translate (- width / 2) 0 $ G.pictures $ concat picsList)
    where (width, picsList) = foldl step (0, []) (bars bc)
          step :: (Float, [[G.Picture]]) -> Bar -> (Float, [[G.Picture]])
          step (accW, accPicsList) b = (width + accW, pics : accPicsList)
              where (width, pics) = drawBar bc b accW

-- | Draw a bar
drawBar :: BarCode -> Bar -> Float -> (Float, [G.Picture])
drawBar bc (PureBar xs) xo = drawBits xs (bitWidth bc) (height bc) xo 0
drawBar bc (DigitBar xs d) xo = drawBitsDigit xs (bitWidth bc) (height bc) d (digitHeightRatio bc) xo 0

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
    where (w, h, pic) = drawBarCode $ toBarCode xs ys cs 120 2 (0.2/1.2)

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
drawBits :: String -> Float -> Float -> Float -> Float -> (Float, [G.Picture])
drawBits [] _ _ _ _ = (0, [])
drawBits (c:xs) w h xo yo = (w + restW, thisPic : restPics)
    where thisPic = drawBit c w h xo yo
          (restW, restPics) = drawBits xs w h (xo + w) yo

-- | Draw bits with a digit.
-- With given '0'/'1' string
-- bit width: bw
-- height: h
-- d: a digit
-- digit height ratio: dhr
-- x offset: xo
-- y offset: yo
drawBitsDigit :: String -> Float -> Float -> Int -> Float -> Float -> Float -> (Float, [G.Picture])
drawBitsDigit xs bw h d dhr xo yo = (totalWidth, digitPic : bitsPics)
    where (totalWidth, bitsPics) = drawBits xs bw bh xo byo
          bh = h - dh
          dh = h * dhr
          -- bar y offset
          byo = yo + (h - bh) / 2

          fontDefaultWidth = 100.0
          fontDefaultHeight = 100.0
          fontGap = 0.2 * dh
          fontHeight = dh - fontGap
          fontZoomRatioW = totalWidth / fontDefaultWidth
          fontZoomRatioH = fontHeight / fontDefaultHeight
          digitPic = G.translate xo (byo - bh / 2 - dh) $ G.scale fontZoomRatioW fontZoomRatioH $ G.text [intToDigit d]

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
