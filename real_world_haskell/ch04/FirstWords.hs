-- For ex3, on page 84
-- Get the first words of each line from a string.
import InteractWith (mainWith)

-- Implement this without loop support
firstWords :: String -> [String]
firstWords [] = []
firstWords cs = let (pre, suf) = break isLineTerminator cs
                    preWords = words pre
                    preFstWord = if null preWords then "" else (head preWords)
                    rest = if null suf then [] else (tail suf)
                in preFstWord : firstWords rest

isLineTerminator c = c == '\n'

main = mainWith (\cs -> unlines (firstWords cs))
