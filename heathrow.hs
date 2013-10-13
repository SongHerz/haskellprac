import Data.List

main = do
    contents <- getContents
    let threes = groupsOf 3 (map read $ words contents)
        roadSystem = map (\[a,b,c] -> Section a b c) threes
        path = optimalPath roadSystem
        pathString = concat $ map (show . fst) path
        pathPrice = sum $ map snd path
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "The price is: " ++ show pathPrice



data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

heathrowToLondon :: RoadSystem
heathrowToLondon = [ Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]


roadStep :: (Path, Path, Int, Int) -> Section -> (Path, Path, Int, Int)
roadStep (pathA, pathB, bestPriceA, bestPriceB) (Section a b c) =
    let forwardPriceToA = bestPriceA + a
        crossPriceToA = bestPriceB + b + c
        forwardPriceToB = bestPriceB + b
        crossPriceToB = bestPriceA + a + c
        (newPathToA, newBestPriceA) = if forwardPriceToA <= crossPriceToA
                        then ((A,a):pathA,       forwardPriceToA)
                        else ((C,c):(B,b):pathB, crossPriceToA)
        (newPathToB, newBestPriceB) = if forwardPriceToB <= crossPriceToB
                        then ((B,b):pathB,       forwardPriceToB)
                        else ((C,c):(A,a):pathA, crossPriceToB)
    in (newPathToA, newPathToB, newBestPriceA, newBestPriceB)


optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestAPath, bestBPath, bestAPrice, bestBPrice) = foldl roadStep ([],[], 0, 0) roadSystem
    in if bestAPrice <= bestBPrice
        then reverse bestAPath
        else reverse bestBPath


groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)
