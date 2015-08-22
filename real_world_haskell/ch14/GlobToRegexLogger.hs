-- Convert global pattern to regex pattern with each step logged.
-- For this example, convertion of ? and * is enough.
module GlobToRegexLogger (globToRegex) where

import Logger

globToRegex :: String -> Logger String
globToRegex cs =
    globToRegex' cs >>= \ds ->
    return ('^' : ds)


globToRegex' :: String -> Logger String
globToRegex' ('?' : cs) =
    record "any" >>
    globToRegex' cs >>= \ds ->
    return ('.' : ds)
globToRegex' ('*' : cs) = 
    record "kleene star" >>
    globToRegex' cs >>= \ds ->
    return (".*" ++ ds)
globToRegex' (c : cs) =
    record [c] >>
    globToRegex' cs >>= \ds ->
    return (c : ds)
globToRegex' [] = return []


r0 = runLogger $ globToRegex ""
r1 = runLogger $ globToRegex "abc"
r2 = runLogger $ globToRegex "?a*b"
