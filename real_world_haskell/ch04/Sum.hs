mySum xs = helper 0 xs
           where helper acc (x:xs) = helper (acc + x) xs
                 helper acc []     = acc
foldSum xs = foldl step 0 xs
             where step acc x = acc + x

niceSum xs = foldl (+) 0 xs
