-- Exercise 2.1, page 11
-- On list
-- This function is O(n) time and O(n) space.
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs@(_ : rest) = xs : suffixes rest
