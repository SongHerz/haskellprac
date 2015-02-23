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
    | prefix `L8.isPrefixOf` str
        = Just (L8.drop (L.length prefix) str)
    | otherwise
        = Nothing

-- "nat" here is short for "natural number"
getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
               Nothing -> Nothing
               Just (num, rest)
                | num <= 0  -> Nothing
                | otherwise -> Just(num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count  = fromIntegral n
                     both@(prefix, _) = L.splitAt count str
                 in if L.length prefix < count
                    then Nothing
                    else Just both

-- Remove leading spaces from the 2nd byte string
skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
    case matchHeader (L8.pack "P5") s of
        Nothing -> Nothing
        Just s1 ->
            case getNat (L8.dropWhile isSpace s1) of
                Nothing -> Nothing
                Just (width, s2) ->
                    case getNat (L8.dropWhile isSpace s2) of
                        Nothing -> Nothing
                        Just (height, s3) ->
                            case getNat (L8.dropWhile isSpace s3) of
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


(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v  >>? f = f v


parseP5_take2 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2 s =
    matchHeader (L8.pack "P5") s        >>?
    \s -> skipSpace ((), s)             >>?
    -- get width
    (getNat . snd)                      >>?
    skipSpace                           >>?
    -- get height
    \(width, s) -> getNat s             >>?
    skipSpace                           >>?
    -- get max grey
    \(height, s) -> getNat s            >>?
    -- skip the byte just after max grey
    \(maxGrey, s) -> getBytes 1 s       >>?
    -- get bitmap pixels
    (getBytes (width * height) . snd)   >>?
    \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)


fstGreymap :: FilePath -> IO (Maybe Greymap)
fstGreymap path = do
    bytes <- L.readFile path
    -- return $ fst `liftM` (parseP5 bytes)
    return $ fst `liftM` (parseP5_take2 bytes)
