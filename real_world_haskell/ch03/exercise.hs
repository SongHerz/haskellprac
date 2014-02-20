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
plaindromeList :: [a] -> [a]
plaindromeList xs = xs ++ reverseList xs []
                    where reverseList [] ys     = ys
                          reverseList (x:xs) ys = reverseList xs (x:ys)


-- For ex5
isPlaindromeList :: Eq a => [a] -> Bool
isPlaindromeList []  = True
isPlaindromeList [x] = False
isPlaindromeList xs
    | head xs == last xs = isPlaindromeList (init (tail xs))
    | otherwise          = False

-- FIXME: check http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html for other implementation of ex4 and ex5
