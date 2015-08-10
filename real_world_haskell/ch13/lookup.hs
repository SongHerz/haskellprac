myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup k ((k1, v1) : xs) = if k == k1
                                 then Just v1
                                 else myLookup k xs
