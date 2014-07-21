-- Only use functions mentioned before this exercise
-- Though, undefined has not been mentioned yet.
lastButOne :: [a] -> a
lastButOne xs = if length xs < 2
                then undefined
                else (if length xs > 2
                      then lastButOne (tail xs)
                      -- length xs == 2
                      else head xs)

-- Pattern match version
lastButOne' :: [a] -> a
lastButOne' []      = undefined
lastButOne' [a]     = undefined
lastButOne' [a,b]   = a
lastButOne' xs      = lastButOne' $ tail xs
