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
splitWith pred xs = let (pre, suf) = break pred xs
                    in if null pre
                       then if not (null suf)
                            then splitWith pred (tail suf)
                            else []
                       else if not (null suf)
                            then pre : splitWith pred (tail suf)
                            else pre : []

main = do
    quickCheck ( splitWith (\x -> x == ',') "abc" == ["abc"])
    quickCheck ( splitWith (\x -> x == ',') "abc," == ["abc"])
    quickCheck ( splitWith (\x -> x == ',') "abc,def" == ["abc", "def"])
    quickCheck ( splitWith (\x -> x == ',') "abc,,def" == ["abc", "def"])
    quickCheck ( splitWith (\x -> x == ',') "abc,,def," == ["abc", "def"])
    quickCheck ( splitWith (\x -> x == ',') ",abc,,def,," == ["abc", "def"])
    quickCheck ( splitWith (\x -> x == ',') ",abc,,def,,ghi" == ["abc", "def", "ghi"])
