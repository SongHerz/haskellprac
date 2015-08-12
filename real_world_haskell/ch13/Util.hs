module Util (split) where

-- | Split a given list by a given delimiter
-- E.g. split ':' ":"   = ["", ""]
--      split ':' "a:b" = ["a", "b"]
--      split ':' ":b"  = ["", "b"]
--      split ':' "a:"  = ["a", ""]
split :: Eq a => a -> [a] -> [[a]]
split delim str = 
        let (before, reminder) = span (/= delim) str
            in
            if null reminder
               then case before of
                        [] -> []
                        x  -> [x]
               else before : case tail reminder of
                                   [] -> [[]]
                                   y -> split delim y
