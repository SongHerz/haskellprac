-- For ex3, on page 84
-- Get the first words of each line from a string.
import InteractWith (mainWith)

-- Implement this without loop support
firstWords :: String -> [String]
firstWords cs = let (pre, suf) = break isLineTerminator cs
                    preWords = words pre
                in if null suf
                   then if null preWords
                        then []
                        else head preWords : []
                   else if null preWords
                        then firstWords (tail suf)
                        else head preWords : firstWords (tail suf)

isLineTerminator c = c == '\n'

main = mainWith (\cs -> unlines (firstWords cs))
