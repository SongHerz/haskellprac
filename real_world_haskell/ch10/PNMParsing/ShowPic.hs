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
import PNM (fstGreymap)
import Parse (parse)
import Greymap (Greymap(..))
import qualified Data.ByteString.Lazy as L
import Data.Word (Word8)

import System.Environment (getArgs)


data ParserType = Naive | Advanced

chunksOf :: Int -> L.ByteString -> [L.ByteString]
chunksOf w bs = helper bs []
    where helper s acc
            | L.null s  = acc
            | otherwise = let (part1, part2) = L.splitAt (fromIntegral w) s
                          in (part1 : helper part2 acc)


flipVertically :: Int -> L.ByteString -> L.ByteString
flipVertically w bs = L.concat . reverse $ chunksOf w bs


gm2pic :: Greymap -> Picture
gm2pic gm = bitmapOfByteString (greyWidth gm) (greyHeight gm) (L.toStrict rgbaData) True
    where convert :: Word8 -> Word8
          convert c = fromIntegral $ (255 *  toInteger c) `quot` (toInteger $ greyMax gm)
          rgbaData = L.concatMap (\w8 -> L.pack $ map convert [fromIntegral (greyMax gm), w8, w8, w8]) $ flipVertically (greyWidth gm) (greyData gm)


chooseParser :: ParserType -> (L.ByteString -> Either String Greymap)
chooseParser Naive = \bytes -> case fstGreymap bytes of
                                  Just gm -> Right gm
                                  Nothing -> Left "Failed to parse with naive parser"
chooseParser Advanced = parse


showGreymap :: Greymap -> IO ()
showGreymap gm = do
    let width = greyWidth gm
        height = greyHeight gm
    let dis = InWindow "Picture" (width , height) (10, 10)
    putStrLn $ "width: " ++ show width
    putStrLn $ "height: " ++ show height
    -- display dis black $ color white $ text "abc"
    display dis black $ gm2pic gm

showGreymapFromFile :: ParserType -> FilePath -> IO ()
showGreymapFromFile pt path = do
    bytes <- L.readFile path
    let egm = chooseParser pt $ bytes
    case egm of
        Left err -> putStrLn err
        Right gm -> showGreymap gm

usage :: String 
usage = "Usage: app [-n] <pgm_file>"

parseArgs :: [String] -> Either String (ParserType, FilePath)
parseArgs [path]    = Right (Advanced, path)
parseArgs [opt0, opt1]
    | opt0 == "-n"  = Right (Naive, opt1)
    | opt1 == "-n"  = Right (Naive, opt0)
    | otherwise     =Left usage
parseArgs _         = Left usage

main = do
    args <- getArgs
    case parseArgs args of
        Left err -> putStrLn err
        Right (pt, path) -> showGreymapFromFile pt path
