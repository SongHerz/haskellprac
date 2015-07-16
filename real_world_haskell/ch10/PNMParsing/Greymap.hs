module Greymap (Greymap(..), greymap2Array) where

import qualified Data.ByteString.Lazy as L
import Data.Array (Array, listArray)
import Data.Word (Word8)

data Greymap = Greymap {
      greyWidth :: Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: L.ByteString
    } deriving (Eq)

instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

greymap2Array :: Greymap -> Array (Int, Int) Word8
greymap2Array gm = listArray ((0, 0), (greyWidth gm - 1, greyHeight gm - 1)) $ L.unpack $ greyData gm
