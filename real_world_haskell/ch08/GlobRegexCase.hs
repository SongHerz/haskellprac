{-
 - This implementation is relative complex.
 - As this turns a alpha into a pair of upper/lower cases.
 - A relative simple implementation is to convert the string and the pattern
 - to lower or upper case first, then match.
 -}
module GolbRegex
    (
        globToRegex
      , matchesGlob
    ) where

import Data.Char (toUpper, toLower, isAlpha)
import Text.Regex.Posix ((=~))

globToRegex :: String -> Bool -> String
globToRegex cs ic = '^' : globToRegex' cs ic ++ "$"

matchesGlob :: FilePath -> String -> Bool -> Bool
matchesGlob name pat ic = name =~ globToRegex pat ic


globToRegex' :: String -> Bool -> String
globToRegex' "" _ = ""

globToRegex' ('*':cs) ic = ".*" ++ globToRegex' cs ic

globToRegex' ('?':cs) ic = '.' : globToRegex' cs ic

globToRegex' ('[':'!':c:cs) ic = "[^" ++ (letterRegex c ic True ++ charClass cs ic)

globToRegex' ('[':c:cs) ic = '[' : letterRegex c ic True ++ charClass cs ic

globToRegex' ('[':_) ic = error "unterminated character class 0"

globToRegex' (c:cs) ic = (concat $ map (\x -> letterRegex x ic False) $ escape c) ++ globToRegex' cs ic


escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"

-- letter, ignore_case, in_char_class -> return value
letterRegex :: Char -> Bool -> Bool -> String
letterRegex c False _     = [c]
letterRegex c True  True
    | isAlpha c = [toUpper c, toLower c]
    | otherwise = [c]
letterRegex c True  False
    | isAlpha c = ['[', toUpper c, toLower c, ']']
    | otherwise = [c]



charClass :: String -> Bool -> String
charClass (']':cs) ic = ']' : globToRegex' cs ic
charClass (c:cs)   ic = letterRegex c ic True ++ charClass cs ic
charClass []       _  = error "unterminated character class 1"
