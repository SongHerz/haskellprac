module PNM (
      Greymap(..)
    , fstGreymap
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

import Control.Monad (liftM)

data Greymap = Greymap {
      greyWidth :: Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: L.ByteString
    } deriving (Eq)

instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
-- This is the original version.
-- As the note in the function added by me, this is not good.
-- Let me rewrite it below.
{-
matchHeader prefix str
    | prefix `L8.isPrefixOf` str
        -- NOTE: It is not necessary to drop spaces,
        --       as prefix will not be prefix of str if there were spaces
        --       ahead.
        = Just (L8.dropWhile isSpace $ L8.drop (L.length prefix) str)
    | otherwise
        = Nothing
-}

matchHeader prefix str
    | prefix `L8.isPrefixOf` str1
        = Just (L8.drop (L.length prefix) str1)
    | otherwise
        = Nothing
    where str1 =  L8.dropWhile isSpace str

-- "nat" here is short for "natural number"
getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s1 of
               Nothing -> Nothing
               Just (num, rest)
                | num <= 0  -> Nothing
                | otherwise -> Just(num, rest)
            where s1 = L8.dropWhile isSpace s

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count  = fromIntegral n
                     both@(prefix, _) = L.splitAt count str
                 in if L.length prefix < count
                    then Nothing
                    else Just both

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)

parseP5 s =
    case matchHeader (L8.pack "P5") s of
        Nothing -> Nothing
        Just s1 ->
            case getNat s1 of
                Nothing -> Nothing
                Just (width, s2) ->
                    case getNat s2 of
                        Nothing -> Nothing
                        Just (height, s3) ->
                            case getNat s3 of
                                Nothing -> Nothing
                                Just (maxGrey, s4)
                                    | maxGrey > 255 -> Nothing
                                    | otherwise ->
                                        case getBytes 1 s4 of
                                            Nothing -> Nothing
                                            Just (_, s5) ->
                                                case getBytes (width * height) s5 of
                                                    Nothing -> Nothing
                                                    Just (bitmap, s6) ->
                                                        Just (Greymap width height maxGrey bitmap, s6)



fstGreymap :: FilePath -> IO (Maybe Greymap)
fstGreymap path = do
    bytes <- L.readFile path
    return $ fst `liftM` (parseP5 bytes)
