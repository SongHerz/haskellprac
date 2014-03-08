-- With foldr we get the list with the right order
filter_foldr p xs = foldr step [] xs
    where step x acc | p x       = x : acc
                     | otherwise = acc


-- If fold from the left, the result list is reversed
filter_foldl p xs = foldl step [] xs
    where step acc x | p x       = x : acc
                     | otherwise = acc


-- In conclution, when the accumulator is a list:
-- with foldr, the resulting list order is kept with original list
-- with foldl, the resulting list order is reversed

-- So we should use foldr to implement map function
map_foldr :: (a -> b) -> [a] -> [b]
map_foldr f xs = foldr step [] xs
    where step x acc = f x : acc


-- Implement foldl with foldr
foldl_foldr :: ( a -> b -> a) -> a -> [b] -> a
foldl_foldr f acc xs = foldr step id xs acc
    where step x g a = g (f a x)
