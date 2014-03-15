import Test.QuickCheck (quickCheck)

-- For ex1, on page 84
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just (tail xs)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)


-- For ex2, on page 84
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _    [] = []
splitWith pred xs = let (pre, suf) = break pred xs
                        rest = if null suf then [] else (tail suf)
                    in if null pre
                       then splitWith pred rest
                       else pre : splitWith pred rest

-- For ex5, on page 98
myConcat :: [[a]] -> [a]
myConcat xses = foldr (\xs acc -> xs ++ acc) [] xses
--
-- Here are some other style of myConcat
myConcat' :: [[a]] -> [a]
myConcat' = foldr (++) []

-- Actually, this implementation also uses foldr to implement (++)
myConcat'' :: [[a]] -> [a]
myConcat'' = foldr (\xs acc -> foldr (:) acc xs) []

-- For ex7, on page 98
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile p (x:xs)
    | p x       = x : myTakeWhile p xs
    | otherwise = []


myTakeWhile' :: (a -> Bool) -> [a] -> [a]
myTakeWhile' p xs = foldr step [] xs
    where step x acc
            | p x       = x : acc
            | otherwise = []

-- For ex8, on page 98
-- The function eq should be an equality function.
-- In other words, the function should fulfill (eq a b) == (eq b a).
--
-- It is very interesting that, with the foldr version, results are
-- not printed immediately with infinite list.
-- But with the span version below, the result can be printed immediately.
-- Why?
-- OK, I guess with foldr version, the accululator will never be returned,
-- and no result would be printed forever. But the span version returns the
-- list immediately as the evaluation goes on, and it is possible to print
-- the list real time.
--
-- I think what I guess is right.
-- The foldr version ends up by stackoverflow exception.
-- The span version consumes constant memory.
myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy eq xs = foldr step [] xs
    where step x []        = [x] : []
          step x (xs:xses)
            | eq x (head xs) = (x:xs) : xses
            | otherwise      = [x] : xs : xses

-- Here is another implementation, with span.
-- This is easier, though not required by the exercise
myGroupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy' _ []  = []
myGroupBy' eq (x:xs) = let (ys,zs)  = span (eq x) xs
                   in (x : ys) : myGroupBy' eq zs


main = do
    quickCheck ( splitWith (\x -> x == ',') "abc" == ["abc"])
    quickCheck ( splitWith (\x -> x == ',') "abc," == ["abc"])
    quickCheck ( splitWith (\x -> x == ',') "abc,def" == ["abc", "def"])
    quickCheck ( splitWith (\x -> x == ',') "abc,,def" == ["abc", "def"])
    quickCheck ( splitWith (\x -> x == ',') "abc,,def," == ["abc", "def"])
    quickCheck ( splitWith (\x -> x == ',') ",abc,,def,," == ["abc", "def"])
    quickCheck ( splitWith (\x -> x == ',') ",abc,,def,,ghi" == ["abc", "def", "ghi"])
