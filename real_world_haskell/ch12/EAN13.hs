-- Refer http://en.wikipedia.org/wiki/International_Article_Number_%28EAN%29
-- For details of EAN-13 barcode.
module EAN13 (encodeDigits) where

import Data.List (foldl', foldl1', group, sort, sortBy, tails)
import Data.Array (Ix, Array(..), listArray, indices, (!), bounds, elems, ixmap)
import Data.Ratio (Ratio, (%))
import Data.Maybe (catMaybes, listToMaybe)
import Control.Applicative ((<$>))
import Greymap (Greymap, greymap2Array)
import Data.Word (Word8)
import qualified Data.Map as M

-- http://www.gs1.org/check-digit-calculator
-- 978354004934(0)
-- Not the same as the book.
-- The version in the book has a bug, check sum 0 will be outputed as 10.
checkDigit :: (Integral a) => [a] -> a
checkDigit ds = (10 - sum products) `mod` 10
    where products = mapEveryOther (*3) (reverse ds)

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f, id])

-- Encoding of digits
leftOddList = [ "0001101"   -- 0
              , "0011001"   -- 1
              , "0010011"   -- 2
              , "0111101"   -- 3
              , "0100011"   -- 4
              , "0110001"   -- 5
              , "0101111"   -- 6
              , "0111011"   -- 7
              , "0110111"   -- 8
              , "0001011"   -- 9
              ]

rightEvenList = map complement <$> leftOddList
    where complement '0' = '1'
          complement '1' = '0'

leftEvenList = map reverse rightEvenList

-- The parity list is for left 6 digits only.
-- And the parity combinations of the left 6 digits determine the 1st digit
-- of the bar code.
-- '0' even parity.
-- '1' odd parity.
-- 
-- In the wikipedia mentioned at the beginning of this module,
-- This table is called "Structure of EAN-13"
--
--                 Parity      1st Digit
leftParityList = [ "111111"     -- 0
                 , "110100"     -- 1
                 , "110010"     -- 2
                 , "110001"     -- 3
                 , "101100"     -- 4
                 , "100110"     -- 5
                 , "100011"     -- 6
                 , "101010"     -- 7
                 , "101001"     -- 8
                 , "100101"     -- 9
                 ]

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0, l - 1) xs
    where l = length xs

leftOddCodes, leftEvenCodes, rightEvenCodes, leftParityCodes :: Array Int String

leftOddCodes = listToArray leftOddList
leftEvenCodes = listToArray leftEvenList
rightEvenCodes = listToArray rightEvenList
leftParityCodes = listToArray leftParityList

outerGuard = "101"
centerGuard = "01010"

-- -------------------------------------
-- foldA version from the book page 273
-- -------------------------------------
-- | Strict left fold, similar to foldl' on lists
-- Use elems of Array module is better.
{-
foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f z a = go z (indices a)
    where go z (j:js) = let z' = f z (a ! j)
                        in z' `seq` go z' js
          go z _ = z
-}

-- | Strict left fold using the first element of the array as its
-- starting value, similar to foldl1 on lists
--
-- This version has a bug that the 1st number would be the initial value,
-- and it will also be traversed another time.
{-
foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f a = foldA f (a ! fst (bounds a)) a
-}
                            

-- -------------------------------------------------------------------
-- foldA version by me, comments from the on line comments referenced
-- -------------------------------------------------------------------

-- | Strict left fold, similar to foldl' on lists
foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f z a = foldl' f z $ elems a

-- | Strict left fold using the first element of the array as its
-- starting value, similar to foldl1 on lists
foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f a = foldl1' f $ elems a


-- | This function computes the barcode with given 12 digits,
-- the last checksum is computed by it.
encodeDigits :: [Int] -> ([String], Int)
encodeDigits s@(first:rest) =
    (outerGuard : lefties ++ centerGuard : righties ++ [outerGuard], checksum)
    -- The book 'splitAt 5' has a bug, it should be '6' as below.
    where (left, right) = splitAt 6 rest
          lefties = zipWith leftEncode (leftParityCodes ! first) left
          righties = map rightEncode (right ++ [checksum])
          checksum = checkDigit s

leftEncode :: Char -> Int -> String
leftEncode '1' = (leftOddCodes !)
leftEncode '0' = (leftEvenCodes !)

rightEncode :: Int -> String
rightEncode = (rightEvenCodes !)



-- -----------------------
-- Barcode Recognization
-- -----------------------
data Bit = Zero | One
           deriving (Eq, Show)

threshold :: (Ix k, Integral a) => Double -> Array k a -> Array k Bit
threshold n a = binary <$> a
    where binary i | i < pivot = Zero
                   | otherwise = One
          pivot = round $ least + (greatest - least) * n
          greatest = fromIntegral $ choose (>) a
          least = fromIntegral $ choose (<) a
          choose f = foldA1 $ \x y -> if f x y then x else y

type Run = Int
type RunLength a = [(Run, a)]

runLength :: Eq a => [a] -> RunLength a
runLength = map rle . group
    where rle xs = (length xs, head xs)

-- Assume the first bit is always 0,
-- the actual bit value could be omitted.
runLengths :: Eq a => [a] -> [Run]
runLengths = map fst . runLength

type Score = Ratio Int

scaleToOne :: [Run] -> [Score]
scaleToOne xs = map divide xs
    where divide d = fromIntegral d / divisor
          divisor = fromIntegral (sum xs)
-- Another version
-- scaleToOne = map (% sum xs) xs

type ScoreTable = [[Score]]
type Digit = Word8

-- "SRL" means "scaled run length".
asSRL :: [String] -> ScoreTable
asSRL = map (scaleToOne . runLengths)

leftOddSRL = asSRL leftOddList
leftEvenSRL = asSRL leftEvenList
rightEvenSRL = asSRL rightEvenList
leftParitySRL = asSRL leftParityList

distance :: [Score] -> [Score] -> Score
distance a b = sum . map abs $ zipWith (-) a b

bestScores :: ScoreTable -> [Run] -> [(Score, Digit)]
bestScores srl ps = take 3 . sort $ scores
    where scores = zip [distance d (scaleToOne ps) | d <- srl] digits
          digits = [0..9]


data Parity a = Even a | Odd a | None a
                deriving (Show)

fromParity :: Parity a -> a
fromParity (Even a) = a
fromParity (Odd a) = a
fromParity (None a) = a

parityMap :: (a -> b) -> Parity a -> Parity b
parityMap f (Even a) = Even $ f a
parityMap f (Odd a) = Odd $ f a
parityMap f (None a) = None $ f a

instance Functor Parity where
    fmap = parityMap

-- Also defined in Data.Function
on :: (a -> a -> b) -> (c -> a) -> c -> c -> b
f `on` g = \x y -> g x `f` g y

compareWithoutParity :: Ord a => Parity a -> Parity a -> Ordering
compareWithoutParity = compare `on` fromParity

-- Decode for a single digit
bestLeft :: [Run] -> [Parity (Score, Digit)]
bestLeft ps = sortBy compareWithoutParity
              ((map Odd (bestScores leftOddSRL ps)) ++
               (map Even (bestScores leftEvenSRL ps)))

-- Decode for a single digit
bestRight :: [Run] -> [Parity (Score, Digit)]
bestRight = map None . bestScores rightEvenSRL

chunkWith :: ([a] -> ([a], [a])) -> [a] -> [[a]]
chunkWith _ [] = []
chunkWith f xs = let (h, t) = f xs
                 in h : chunkWith f t

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = chunkWith $ splitAt n

-- E.g.
-- let digits = encodeDigits $ map digitToInt "978013211467"
-- let input = zip (runLengths . concat $ fst digits) (cycle [Zero, One])
-- candidateDigits input
--
-- [[Odd 7,Even 1,Even 2,Odd 3,Even 4,Odd 8],[Even 8,Odd 0,Odd 1,Odd 2,Even
-- 6,Even 7],[Even 0,Even 1,Odd 2,Odd 4,Odd 6,Even 9],[Odd 1,Odd 0,Even
-- 1,Odd 2,Even 2,Even 4],[Even 3,Odd 4,Odd 5,Even 7,Even 0,Odd 1],[Odd
-- 2,Even 0,Odd 1,Even 1,Even 2,Odd 4],[None 1,None 0,None 2],[None 1,None
-- 0,None 2],[None 4,None 2,None 5],[None 6,None 8,None 2],[None 7,None
-- 3,None 8],[None 7,None 3,None 8]]
candidateDigits :: RunLength Bit -> [[Parity Digit]]
candidateDigits ((_, One):_) = []
candidateDigits rle | length rle < 59 = []
candidateDigits rle
    | any null match = []
    | otherwise = map (map (fmap snd)) match
    where match = map bestLeft left ++ map bestRight right :: [[Parity (Score, Digit)]]
          -- Drop left guard, take 6 left digits, and split into chunks of
          -- 4.
          left = chunksOf 4 . take (4 * 6) . drop 3 $ helperRunLengths :: [[Run]]
          -- Drop (left guard + 6 left digits + inner guard), take 6 right
          -- digits, and split into chunks of 4.
          right = chunksOf 4 . take (4 * 6) . drop (3 + 4 * 6 + 5) $ helperRunLengths :: [[Run]]
          helperRunLengths = map fst rle

type CheckMap a = M.Map Digit [a]
type DigitMap = CheckMap Digit
type ParityMap = CheckMap (Parity Digit)

updateMap :: Parity Digit   -- ^ new digit
          -> Digit          -- ^ existing key
          -> [Parity Digit] -- ^ existing digit sequence
          -> ParityMap      -- ^ map to update
          -> ParityMap
updateMap digit key seq = insertMap key (fromParity digit) (digit:seq)

insertMap :: Digit -> Digit -> [a] -> CheckMap a -> CheckMap a
insertMap key digit val m = val `seq` M.insert key' val m
    where key' = (key + digit) `mod` 10

useDigit :: ParityMap -> ParityMap -> Parity Digit -> ParityMap
useDigit old new digit = new `M.union` M.foldWithKey (updateMap digit) M.empty old

incorporateDigits :: ParityMap -> [Parity Digit] -> ParityMap
incorporateDigits old digits = foldl' (useDigit old) M.empty digits

finalDigits :: [[Parity Digit]] -> ParityMap
finalDigits = foldl' incorporateDigits (M.singleton 0 [])
            . mapEveryOther (map (fmap (*3)))

firstDigit :: [Parity a] -> Digit
firstDigit = snd
           . head
           . bestScores leftParitySRL
           . runLengths
           . map parityBit
           . take 6
           where parityBit (Even _) = Zero
                 parityBit (Odd _) = One

addFirstDigit :: ParityMap -> DigitMap
addFirstDigit = M.foldWithKey updateFirst M.empty

updateFirst :: Digit -> [Parity Digit] -> DigitMap -> DigitMap
updateFirst key seq = insertMap key digit (digit:renormalize qes)
    where renormalize = mapEveryOther (`div` 3) . map fromParity
          digit = firstDigit qes
          qes = reverse seq

buildMap :: [[Parity Digit]] -> DigitMap
buildMap = M.mapKeys (10 -)
         . addFirstDigit
         . finalDigits

solve :: [[Parity Digit]] -> [[Digit]]
solve [] = []
solve xs = catMaybes $ map (addCheckDigit m) checkDigits
    where checkDigits = map fromParity $ last xs
          m = buildMap (init xs)
          addCheckDigit m k = (++[k]) <$> M.lookup k m

type RGB = (Word8, Word8, Word8)
type Pixmap = Array (Int, Int) RGB

luminance :: RGB -> Word8
luminance (r, g, b) = round(r' * 0.30 + g' * 0.59 + b' * 0.11)
    where [r', g',  b'] = map fromIntegral [r, g, b]

withRow :: Int -> Pixmap -> (RunLength Bit -> a) -> a
withRow n greymap f = f . runLength . elems $ posterized
    where posterized = threshold 0.4 . fmap luminance . row n $ greymap

row :: (Ix a, Ix b) => b -> Array (a, b) c -> Array a c
row j a = ixmap (l, u) project a
    where project i = (i, j)
          ((l, _), (u, _)) = bounds a

findMatch :: [(Run, Bit)] -> Maybe [[Digit]]
findMatch = listToMaybe
            . filter (not . null)
            . map (solve . candidateDigits)
            . tails

findEAN13 :: Pixmap -> Maybe [Digit]
findEAN13 pixmap = withRow center pixmap (fmap head . findMatch)
    where (_, (maxX, _)) = bounds pixmap
          center = (maxX + 1) `div` 2
