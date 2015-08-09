module Decoder (decodePic) where

import Data.Array (array)
import EAN13 (findEAN13, Pixmap, RGB, Digit)
import Codec.Picture
import Codec.Picture.Types

dummyPixmap = array ((0,0), (0,0)) [((0, 0), (1, 1, 1))]

dynWidth = dynamicMap imageWidth
dynHeight = dynamicMap imageHeight

colorType :: DynamicImage -> String
colorType (ImageY8 _) = "Y8"
colorType (ImageY16 _) = "Y16"
colorType (ImageYF _) = "YF"
colorType (ImageYA8 _) = "YA8"
colorType (ImageYA16 _) = "YA16"
colorType (ImageRGB8 _) = "RGB8"
colorType (ImageRGB16 _) = "RGB16"
colorType (ImageRGBF _) = "RGBF"
colorType (ImageRGBA8 _) = "RGBA8"
colorType (ImageRGBA16 _) = "RGBA16"
colorType (ImageYCbCr8 _) = "YCbCr8"
colorType (ImageCMYK8 _) = "CMYK8"
colorType (ImageCMYK16 _) = "CMYK16"

internalDIToPm :: Pixel a => Image a -> (a -> RGB) -> Pixmap
internalDIToPm img f = array ((0, 0), (w - 1, h - 1)) [((x, y), f $ pixelAt img x y) | x <- [0..(w - 1)], y <- [0..(h - 1)] ]
    where w = imageWidth img
          h = imageHeight img

pixelRGB8ToRGB (PixelRGB8 r g b) = (r, g, b) :: RGB
-- https://en.wikipedia.org/wiki/YCbCr#JPEG_conversion
-- pixelYCbCr8ToRGB (PixelYCbCr8 y cb cr) = (floor fr, floor fg, floor fb) :: RGB
--     where [fy, fcb, fcr] = map toRational [y, cb, cr]
--           fr = fy + 1.402 * (fcr - 128)
--           fg = fy - 0.34414 * (fcb - 128) - 0.71414 * (fcr - 128)
--           fb = fy + 1.772 * (fcb - 128)
pixelRGBA8ToRGB (PixelRGBA8 r g b a) = (floor fr, floor fg, floor fb) :: RGB
    where [fr, fg, fb] = map ((k*) . toRational) [r, g, b]
          k = (toRational a) / 255


dynamicImageToPixmap :: DynamicImage -> Either String Pixmap
dynamicImageToPixmap (ImageRGB8 img) = Right $ internalDIToPm img pixelRGB8ToRGB
dynamicImageToPixmap (ImageRGBA8 img) = Right $ internalDIToPm img $ pixelRGBA8ToRGB
dynamicImageToPixmap (ImageYCbCr8 img) = Right $ internalDIToPm img $ pixelRGB8ToRGB . convertPixel
dynamicImageToPixmap dyimg =
        Left $ concat ["Unsupported image ", colorType dyimg, " ", show (dynWidth dyimg), "x", show (dynHeight dyimg)]

findAndShowEAN13FromDynamicImage :: DynamicImage -> IO ()
findAndShowEAN13FromDynamicImage img =
    case dynamicImageToPixmap img of
        Right pm -> putStrLn $ show $ findEAN13 pm
        Left err -> putStrLn err

decodePic :: FilePath -> IO ()
decodePic p = do
    may_img <- readImage p
    case may_img of
        Right dyimg -> findAndShowEAN13FromDynamicImage dyimg
        Left err -> putStrLn err

