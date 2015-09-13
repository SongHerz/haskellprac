-- Rewrite ch10_revisit.hs with MaybeState Monad defined in this chapter.

import MaybeState
import Data.Char (isSpace, isDigit)

type Consume a = MaybeState String a

skipSpaces :: Consume ()
skipSpaces = do
    s <- get
    let (_, non_spcs) = span isSpace s
    put non_spcs
    return ()

getInt :: Consume Int
getInt = do
    s <- get
    let (digits, rest) = span isDigit s
    put rest
    case digits of
        [] -> returnBad
        _  -> return $ read digits

getX :: Consume Char
getX = do
    s <- get
    if (not . null) s && (head s `elem` "xX")
        then do
            put $ tail s
            return $ head s
        else returnBad

parseAction :: Consume (Int, Int)
parseAction = do
    skipSpaces
    d0 <- getInt
    skipSpaces
    x <- getX
    skipSpaces
    d1 <- getInt
    return (d0, d1)

parse :: String -> Maybe (Int, Int)
parse s = evalMaybeState parseAction s

r0 = parse "123"
r1 = parse "123x456"
r2 = parse "   456   X 324  "
