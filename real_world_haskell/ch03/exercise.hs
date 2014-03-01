import Data.List (sortBy)
import Data.Ord  (comparing)
import Test.QuickCheck (quickCheck)

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

-- For ex9/ex10
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

-- For ex11
directions :: (Num a, Ord a) => [Point a] -> [Direction]
directions (a:b:c:xs) = direction a b c : directions (b:c:xs)
directions _          = []

-- For ex12
-- The algorithm is from:
-- http://en.wikipedia.org/wiki/Graham_scan
-- The algorithm is to find convex hull be given series of 2d points

-- Find the lowest point from a given list
lowest :: (Num a, Ord a) => [Point a] -> Point a
lowest ps = innerLowest (head ps) (tail ps)
            where innerLowest (Point x0 y0) [] = Point x0 y0
                  innerLowest (Point x0 y0) (Point x1 y1:ps)
                      | y0 < y1   = innerLowest (Point x0 y0) ps
                      | y0 > y1   = innerLowest (Point x1 y1) ps
                      -- y0 == y1, check x
                      | x0 < x1   = innerLowest (Point x0 y0) ps
                      -- y0 == y1 and x0 >= x1, pick point 1
                      | otherwise = innerLowest (Point x1 y1) ps

-- Calculate the angle of one point relative to a start point
-- When the point is the same as start point, the angle is defined as (-pi)
angle :: (RealFloat a) => Point a -> Point a -> a
angle (Point x y) (Point startX startY) =
    let dy = y - startY
        dx = x - startX
    in if dy == 0 && dy == 0
       then (-pi)
       else atan2 dy dx


-- After the algorithm, the points are clock-wise ordered
grahamScan :: (RealFloat a, Ord a) => [Point a] -> [Point a]
grahamScan ps = reverse $ findBoundary newps []
                where startPoint = lowest ps
                      newps = sortBy (comparing (\p -> angle p startPoint)) ps 
                      -- ps: points, bps: boundary points
                      findBoundary []     bps  = bps
                      findBoundary (p:ps) []   = findBoundary ps [p]
                      findBoundary (p:ps) [bp] = findBoundary ps (p:[bp])
                      findBoundary (p:ps) (bp1:bp0:bps)
                        | direction bp0 bp1 p == RightTurn = findBoundary (p:ps) (bp0:bps)
                        | otherwise                        = findBoundary ps (p:bp1:bp0:bps)
                                                                

-- Expected result is: [Point (-1.0) 1.0,Point 2.0 2.0,Point 3.0 0.0,Point 0.0 (-3.0)]
showConvexHull = grahamScan [ Point 2 2, Point (-1) 1, Point 3 0, Point 1 (-1.5), Point 0 (-3)]

-- Expected result is: [Point (-1.0) 1.0,Point 2.0 2.0,Point 3.0 0.0,Point 0.0 (-3.0),Point 0.0 (-3.0)]
showConvexHull' = grahamScan [ Point 0 (-3), Point 2 2, Point (-1) 1, Point 3 0, Point 1 (-1.5), Point 0 (-3)]

isConvexHull :: (RealFloat a, Ord a) => [Point a] -> Bool
isConvexHull ps = all (\d -> d == LeftTurn || d == Stright) (directions ps)

checkIsConvexHull :: [(Double, Double)] -> Bool
checkIsConvexHull ps = isConvexHull $ grahamScan [Point x y | (x, y) <- ps]

main = quickCheck checkIsConvexHull 
