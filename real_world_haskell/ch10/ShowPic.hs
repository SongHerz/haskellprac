-- {-
-- When run with ghci, ghci crashes with libGL error:
-- 
-- Loading package OpenGLRaw-1.3.0.0 ... linking ... done.
-- Loading package GLURaw-1.3.0.0 ... linking ... done.
-- Loading package array-0.4.0.1 ... linking ... done.
-- Loading package deepseq-1.3.0.1 ... linking ... done.
-- Loading package OpenGL-2.8.0.0 ... linking ... done.
-- Loading package containers-0.5.0.0 ... linking ... done.
-- Loading package GLUT-2.4.0.0 ... linking ... done.
-- Loading package bytestring-0.10.0.2 ... linking ... done.
-- Loading package binary-0.6.4.0 ... linking ... done.
-- Loading package bmp-1.2.5.2 ... linking ... done.
-- Loading package gloss-1.8.1.2 ... linking ... done.
-- libGL error: failed to load driver: swrast
-- freeglut (<interactive>): stroke font 0x41a7ae88 not found
-- 
-- I have no idea at all.
-- But this works when compiling.
-- -}

import Graphics.Gloss
import PNM
import qualified Data.ByteString.Lazy as L
import Data.Word (Word8)

gm2pic :: Greymap -> Picture
gm2pic gm = bitmapOfByteString (greyWidth gm) (greyHeight gm) (L.toStrict rgbaData) True
    where convert :: Word8 -> Word8
          convert c = fromIntegral $ (255 *  toInteger c) `quot` (toInteger $ greyMax gm)
          rgbaData = L.concatMap (\w8 -> L.pack $ map convert [fromIntegral (greyMax gm), w8, w8, w8]) $ greyData gm

main = do
    mgm <- fstGreymap "baboon.pgm"
    let (width, height, pic) = case mgm of
                                  Nothing -> (0, 0, blank)
                                  Just gm -> (greyWidth gm, greyHeight gm, gm2pic gm)

    let dis = InWindow "Picture" (width , height) (10, 10)
    -- display dis black $ color white $ text "abc"
    display dis black $ pic
