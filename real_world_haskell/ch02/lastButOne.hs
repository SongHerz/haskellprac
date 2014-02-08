-- Only use functions mentioned before this exercise
-- Though, undefined has not been mentioned yet.
lastButOne :: [a] -> a
lastButOne xs = if length xs < 2
                then undefined
                else (if length xs > 2
                      then lastButOne (tail xs)
                      -- length xs == 2
                      else head xs)
