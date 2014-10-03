{-
 - This implementation is relative complex.
 - As this turns a alpha into a pair of upper/lower cases.
 - A relative simple implementation is to convert the string and the pattern
 - to lower or upper case first, then match.
 -}
module GlobRegexCaseEither
    (
        globToRegex
      , matchesGlob
    ) where

import Data.Char (toUpper, toLower, isAlpha)
import Text.Regex.Posix ((=~))

type GlobError = String

globToRegex :: String -> Bool -> Either GlobError String
globToRegex cs ic =
    case globToRegex' cs ic of
        Right subPtn -> Right $ '^' : subPtn ++ "$"
        Left  err    -> Left err

matchesGlob :: FilePath -> String -> Bool -> Either GlobError Bool
matchesGlob name pat ic =
    case globToRegex pat ic of
        Right ptn -> Right $ name =~ ptn
        Left  err -> Left err


globToRegex' :: String -> Bool -> Either GlobError String
globToRegex' "" _ = Right ""

globToRegex' ('*':cs) ic =
    case globToRegex' cs ic of
        Right subPtn -> Right $ ".*" ++ subPtn
        Left  err    -> Left err
                            
globToRegex' ('?':cs) ic =
    case globToRegex' cs ic of
        Right subPtn -> Right $ '.' : subPtn
        Left  err    -> Left err

globToRegex' ('[':'!':c:cs) ic = 
    case charClass cs ic of
        Right subPtn -> Right $ "[^" ++ (letterRegex c ic True ++ subPtn)
        Left  err    -> Left err

globToRegex' ('[':c:cs) ic =
    case charClass cs ic of
        Right subPtn -> Right $ '[' : letterRegex c ic True ++ subPtn
        Left  err    -> Left err

globToRegex' ('[':_) ic = Left "unterminated character class 0"

globToRegex' (c:cs) ic =
    case globToRegex' cs ic of
        Right subPtn -> Right $ (concat $ map (\x -> letterRegex x ic False) $ escape c) ++ subPtn
        Left  err    -> Left err


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



charClass :: String -> Bool -> Either GlobError String
charClass (']':cs) ic =
    case globToRegex' cs ic of
        Right subPtn -> Right $ ']' : subPtn
        Left  err    -> Left err

charClass (c:cs)   ic =
    case charClass cs ic of
        Right subPtn -> Right $ letterRegex c ic True ++ subPtn
        Left  err    -> Left err

charClass []       _  = Left "unterminated character class 1"
