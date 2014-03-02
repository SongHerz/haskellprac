-- For ex3, on page 84
-- Get the first words of each line from a string.
import InteractWith (mainWith)

-- Implement this without loop support
-- Of course, map can be used to implement it also.
-- It is said, map has been mentioned in the book until now, but I cannot remember
firstWords :: String -> [String]
firstWords [] = []
firstWords cs = let (pre, suf) = break isLineTerminator cs
                    preWords = words pre
                    preFstWord = if null preWords then "" else (head preWords)
                    rest = if null suf then [] else (tail suf)
                in preFstWord : firstWords rest

isLineTerminator c = c == '\n'


-- Another version with map
firstWords' :: String -> [String]
firstWords' cs = map mayEmptyHead (map words (lines cs))
                 where mayEmptyHead [] = []
                       mayEmptyHead xs = head xs

main = mainWith (\cs -> unlines (firstWords' cs))
