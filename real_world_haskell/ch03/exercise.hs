import Data.List (sortBy)
import Data.Ord  (comparing)

-- For ex1 and ex2
listLength :: [a] -> Int
listLength []     = 0
listLength (_:xs) = 1 + listLength xs


-- For ex3
listSum :: Num a => [a] -> a
listSum []     = 0
listSum (x:xs) = x + listSum xs

meanList :: Fractional a => [a] -> a
meanList xs = listSum xs / fromIntegral ( listLength xs)


-- For ex4
reverseList :: [a] -> [a]
reverseList xs = innerReverse xs []
                 where innerReverse [] ys     = ys
                       innerReverse (x:xs) ys = innerReverse xs (x:ys)

plaindromeList :: [a] -> [a]
plaindromeList xs = xs ++ reverseList xs


-- For ex5
isPlaindromeList :: Eq a => [a] -> Bool
isPlaindromeList []  = True
isPlaindromeList [x] = False
isPlaindromeList xs
    | head xs == last xs = isPlaindromeList (init (tail xs))
    | otherwise          = False

-- The above implementation on ex5 is bad, because the time complexity on 
-- last and init is O(n) ?? NOT SURE YET, BUT PROBABLY RIGHT.
-- The reverse version is much more faster.
isPlaindromeList' :: Eq a => [a] -> Bool
isPlaindromeList' xs
    | length xs `mod` 2 == 1 = False
    | otherwise              = xs == reverseList xs


-- For ex6
sortListLen :: [[a]] -> [[a]]
sortListLen xses = sortBy (\xs ys -> compare (length xs) (length ys)) xses

-- Another version with comparing
sortListLen' :: [[a]] -> [[a]]
sortListLen' xses = sortBy (comparing length) xses

-- This is another version from Kenneth Hoste on the comment of ex6
-- http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html
-- With this solution, when a long and a short lists are compared, the long list will
-- not be traversed entirely.
sortListLen'' :: [[a]] -> [[a]]
sortListLen'' xses = sortBy compareListByLength xses
                  where compareListByLength [] [] = EQ
                        compareListByLength _ []  = GT
                        compareListByLength [] _  = LT
                        compareListByLength (_:xs) (_:ys) = compareListByLength xs ys


-- For ex7
intersperse :: a -> [[a]] -> [a]
intersperse _   []        = []
intersperse _   [xs]      = xs
intersperse sep (xs:yses) = xs ++ (sep : intersperse sep yses)

-- For ex8
-- Refer Tree.hs: height function.

-- For ex9
data Point a = Point a a deriving (Show)
data Direction = LeftTurn | RightTurn | Stright deriving (Show, Eq)

direction :: (Num a, Ord a) => Point a -> Point a -> Point a -> Direction
direction (Point ax ay) (Point bx by) (Point cx cy)
    | cross < 0  = RightTurn
    | cross == 0 = Stright
    | cross > 0  = LeftTurn
    where v0 = (bx - ax, by - ay)
          v1 = (cx - bx, cy - by)
          -- v0 x v1 = v0.x * v1.y - v0.y * v1.x
          cross = (fst v0 * snd v1) - (snd v0 * fst v1)
