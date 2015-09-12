-- Rewrite ch10_revisit.hs with State Monad defined in this chapter.

import State
import Data.Char (isSpace, isDigit)

type Consume a = State String (Maybe a)

skipSpaces :: Consume ()
skipSpaces = do
    s <- get
    let (_, non_spcs) = span isSpace s
    put non_spcs
    return $ Just ()

getInt :: Consume Int
getInt = do
    s <- get
    let (digits, rest) = span isDigit s
    put rest
    return $ case digits of
                 [] -> Nothing
                 _  -> Just (read digits)

getX :: Consume Char
getX = do
    s <- get
    if (not . null) s && (head s `elem` "xX")
        then do
            put $ tail s
            return $ Just $ head s
        else return Nothing

parseAction :: Consume (Int, Int)
parseAction = do
    skipSpaces
    d0 <- getInt
    skipSpaces
    x <- getX
    skipSpaces
    d1 <- getInt
    return $ getResult d0 x d1
    where getResult :: Maybe Int -> Maybe Char -> Maybe Int -> Maybe (Int, Int)
          getResult d0 x d1 = do
              d0' <- d0
              _   <- x
              d1' <- d1
              return (d0', d1')

parse :: String -> Maybe (Int, Int)
parse s = evalState parseAction s

r0 = parse "123"
r1 = parse "123x456"
r2 = parse "   456   X 324  "
