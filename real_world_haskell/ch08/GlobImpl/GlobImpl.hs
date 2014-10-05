{-
 - This is a glob regexpr implementated by myself.
 -}
module GlobImpl (matchesGlob) where

import Data.List (tails)
import RNode

matchesGlob :: String -> String -> Bool
matchesGlob name pat = matchesGlob' name $ preProcess pat

matchesGlob' :: String -> [RNode] -> Bool
matchesGlob' xs (RStar:rs)  = any (`matchesGlob'` rs) $ tails xs
matchesGlob' (_:xs) (RQuestion:rs) = matchesGlob' xs rs
matchesGlob' (x:xs) (RChar c:rs) = if x == c
                                   then matchesGlob' xs rs
                                   else False
matchesGlob' (x:xs) (RClass inclusive chars:rs) =
    if current_match
    then matchesGlob' xs rs
    else False
    where x_in_chars = x `elem` chars
          current_match = if inclusive
                          then x_in_chars
                          else not x_in_chars

-- Write like this to guarantee neither list is empty here.
-- If both lists are non-empty, non-exhausive error will be raised.
matchesGlob' [] [] = True
matchesGlob' [] _  = False
matchesGlob' _  [] = False
