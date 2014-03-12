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


main = do
    quickCheck ( splitWith (\x -> x == ',') "abc" == ["abc"])
    quickCheck ( splitWith (\x -> x == ',') "abc," == ["abc"])
    quickCheck ( splitWith (\x -> x == ',') "abc,def" == ["abc", "def"])
    quickCheck ( splitWith (\x -> x == ',') "abc,,def" == ["abc", "def"])
    quickCheck ( splitWith (\x -> x == ',') "abc,,def," == ["abc", "def"])
    quickCheck ( splitWith (\x -> x == ',') ",abc,,def,," == ["abc", "def"])
    quickCheck ( splitWith (\x -> x == ',') ",abc,,def,,ghi" == ["abc", "def", "ghi"])
