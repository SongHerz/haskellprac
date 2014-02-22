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


