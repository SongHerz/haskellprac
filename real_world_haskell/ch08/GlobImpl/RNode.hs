-- A glob regex is converted to [RNode] first
module RNode (RNode(..), preProcess) where

data RNode = RStar          -- *
           | RQuestion      -- ?
           | RChar Char     -- A character
           | RClass {       -- A set of characters E.g. [abc] or [!abc]
                inclusive   :: Bool
              , chars       :: [Char]
             }
           deriving (Eq, Show)



isRegexChar :: Char -> Bool
isRegexChar c = c `elem` "*?[\\"

-- Convert a String to [RNode]
preProcess :: String -> [RNode]
preProcess []         = []
preProcess ('\\':c:xs) = RChar c : preProcess xs
preProcess ('*':xs)    = RStar : preProcess xs
preProcess ('?':xs)    = RQuestion : preProcess xs
preProcess ('[':xs)    = let (rClass, remaining) = preProcClass xs
                         in rClass : preProcess remaining
preProcess (c:xs)      = RChar c : preProcess xs

type CharSet = [Char]

-- This process a string, and return an (RClass, remaining string) pair
preProcClass :: String -> (RNode, String)
preProcClass xs       = (   RClass { inclusive = not isExclusive,
                                   chars = charSet}
                          , remaining)
                        where isExclusive = not (null xs) && head xs == '!'
                              xs' = if isExclusive then tail xs else xs
                              (remaining, charSet) = preProcClass' xs' [] 

-- From a string get char list of a character class
preProcClass' :: String -> CharSet -> (String, CharSet)
preProcClass' (']':xs) cs    = (xs, cs)
preProcClass' ('\\':c:xs) cs = preProcClass' xs (c:cs)
preProcClass' (c:xs)   cs    = preProcClass' xs (c:cs)
preProcClass' []       _     = error "unterminated character class"

