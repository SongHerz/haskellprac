{-# LANGUAGE ScopedTypeVariables #-}

module Parse where

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Int (Int64)
import Control.Applicative ((<$>))

data Section = Section { 
       options :: [String]
     , values :: [String]
     }
     deriving (Show)

data Ini = Ini { sections :: [Section] } deriving (Show)


data ParseState = ParseState {
      string :: L8.ByteString
    , offset :: Int64
    }

newtype Parse a = Parse {
    runState :: ParseState -> Either String (a, ParseState)
    }

identity :: a -> Parse a
identity a = Parse $ \state -> Right (a, state)

(==>) :: forall a b. Parse a -> (a -> Parse b) -> Parse b
parseA ==> funcB = Parse chain
    where chain :: ParseState -> Either String (b, ParseState)  -- The signature is actually (runState of Parse b)
          chain initState =
              case runState parseA initState of
                  Left err ->
                      Left err

                  Right (resultA, newState) ->
                      runState (funcB resultA) newState

(==>&) :: Parse a -> Parse b -> Parse b
parseA ==>& parseB = parseA ==> \_ -> parseB

instance Functor Parse where
    -- (a -> b) -> Parse a -> Parse b
    fmap f parseA = parseA ==> \resultA -> identity (f resultA)

getState :: Parse ParseState
getState = Parse $ \state -> Right (state, state)

putState :: ParseState -> Parse ()
putState newState = Parse $ \_ -> Right ((), newState)


bail :: String -> Parse a
bail err = Parse $ \state -> Left $
             "Error: " ++ err ++ ", offset: " ++ (show $ offset state)

peekChar :: Parse (Maybe Char)
peekChar = (fmap fst . L8.uncons . string) <$> getState

parseChar :: Parse Char
parseChar = getState ==> \state ->
            case L8.uncons (string state) of
                Nothing -> bail "No more input"
                Just (c, rest)  ->
                    putState newState ==>&
                    identity c
                    where newState = state { string = rest, offset = newOffset }
                          newOffset = offset state + 1

parseWhile :: (Char -> Bool) -> Parse String
parseWhile p =
    (fmap p <$> peekChar) ==> \mc ->
    if mc == Just True
    then parseChar ==> \c -> (c:) <$> parseWhile p
    else identity []

whites = " \t\r\n"
lineTerms = "\r\n"

isWhite :: Char -> Bool
isWhite = (`elem` whites)

isLineTerm :: Char -> Bool
isLineTerm = (`elem` lineTerms)

parseIsEOF :: Parse Bool
parseIsEOF = (== Nothing) <$> peekChar

parseSpaces :: Parse String
parseSpaces = parseWhile isWhite

-- Assume no space ahead
parseComment :: Parse String
parseComment =
    peekChar ==> \mc ->
    if mc == Just '#'
    then parseWhile (not . isLineTerm)
    else identity ""

-- Assume no space ahead
parseSectionHeader :: Parse (Maybe String)
parseSectionHeader =
    peekChar ==> \mc ->
    if mc == Just '['
    then parseChar ==>&
         parseWhile (/= ']') ==> \sect ->
         -- Consume the last ']'
         parseChar ==>&
         (identity $ Just sect)
     else identity Nothing

-- Assume no space ahead
parseOption :: Parse (Maybe (String, String))
parseOption =
    -- get option name
    parseWhile (not . (\c -> isWhite c || c == '[')) ==> \optname ->
    if null optname
    -- no option name found
    then identity Nothing
    -- find option
    else parseSpaces ==>&
        ((fmap (== '=')) <$> peekChar) ==> \findEq ->
        if findEq /= Just True
        then bail "Invalid option, due to no '=' found"
        else parseChar ==>& -- consume the '='
             parseWhile (not . isLineTerm) ==> \optval ->
             identity $ Just (optname, optval)

runParse :: L8.ByteString -> Parse a -> Either String a
runParse bs parser = case runState parser (ParseState bs 0) of
                         Left err -> Left err
                         Right (result, _) -> Right result
