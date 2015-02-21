{-
- Test row order of bitmapOfByteString constructed picture.
-
- The image generated should be:
- darkest ... gray
- ...
- gray    ... lightest
-
- But the picture shown is:
- gray    ... lightest
- ...
- darkest ... gray
-
- Of course, it is confirmed that the byte string passed to
- bitmapOfByteString should be in upside down order, which is the same as
- a BMP format.
-}

import Graphics.Gloss
import PNM
import qualified Data.ByteString.Lazy as L
import Data.Word (Word8)

gWidth = 200
gHeight = 100

lbrgba :: Int -> Int -> L.ByteString
lbrgba w h = L.concat $ map aLine [0 .. (h - 1)]
    where maxColor = w + h
          color :: Int -> Int -> Word8
          color row col = fromIntegral $ (255 * (row + col)) `quot` (w + h)
          aLine row = L.pack . concat $ map (\col -> [255, color row col, color row col, color row col]) [0 .. (w - 1)] 

pic :: Int -> Int -> Picture
pic w h = bitmapOfByteString w h (L.toStrict $ lbrgba w h) True

main = do
    let dis = InWindow "Picture" (gWidth , gHeight) (10, 10)
    display dis black $ pic gWidth gHeight
