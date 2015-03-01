{-# LANGUAGE ScopedTypeVariables #-}

module Parse where

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Int (Int64)
import Data.Char (isSpace)
import Control.Applicative ((<$>))

data Section = Section { 
         name :: String
       , optionValues :: [(String, String)]
     }
     deriving (Eq, Show)

data Ini = Ini { sections :: [Section] } deriving (Show)


data ParseState = ParseState {
      string :: L8.ByteString
    , offset :: Int64
    }

newtype Parse a = Parse {
    runState :: ParseState -> Either String (a, ParseState)
    }

stripStart :: String -> String
stripStart str = dropWhile isSpace str

stripEnd :: String -> String
stripEnd str = reverse . stripStart . reverse $ str

strip :: String -> String
strip str = stripStart . stripEnd $ str

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

parseComments :: Parse [String]
parseComments =
    parseSpaces ==>&
    parseComment ==> \cmt ->
    if null cmt
    then identity []
    else (cmt:) <$> parseComments

-- Assume no space ahead
parseSectionHeader :: Parse String
parseSectionHeader =
    peekChar ==> \mc ->
    if mc == Just '['
    then parseChar ==>&
         (strip <$> parseWhile (/= ']')) ==> \sect ->
         -- Consume the last ']'
         parseChar ==>&
         checkAndReturnSectHeader sect
     else bail "Invalid section header"
     where checkAndReturnSectHeader :: String -> Parse String
           checkAndReturnSectHeader sect =
               if null sect
               then bail "Empty section header"
               else identity sect


-- Assume no space ahead
parseOption :: Parse (Maybe (String, String))
parseOption =
    -- get option name
    (strip <$> parseWhile (not . (\c -> isWhite c || c `elem` "[="))) ==> \optname ->
    if null optname
    -- no option name found
    then identity Nothing
    -- find option
    else parseSpaces ==>&
        ((fmap (== '=')) <$> peekChar) ==> \findEq ->
        if findEq /= Just True
        then bail "Invalid option, due to no '=' found"
        else parseChar ==>& -- consume the '='
             (strip <$> parseWhile (not . (\c -> isLineTerm c || c == '#'))) ==> \optval ->
             identity $ Just (optname, optval)

parseOptions :: Parse [(String, String)]
parseOptions = 
    -- Ignore comments
    parseComments ==>&
    parseOption ==> \moptval ->
    case moptval of
        Nothing ->
            identity []
        Just optval ->
            (optval:) <$> parseOptions

-- Parse a whole section, 
-- excluding comments before a section,
-- including comments after a section
parseSection :: Parse Section
parseSection =
    parseSectionHeader ==> \sect ->
    if null sect
    then bail "Invalid section header"
    else parseOptions ==> \optvals ->
         identity $ Section sect optvals


-- Parse all sections in a ini file.
parseSections :: Parse [Section]
parseSections =
    parseComments ==>&
    parseIsEOF ==> \eof ->
    if eof
    then identity []
    else parseSection ==> \section ->
         (section:) <$> parseSections

runParse :: L8.ByteString -> Parse a -> Either String a
runParse bs parser = case runState parser (ParseState bs 0) of
                         Left err -> Left err
                         Right (result, _) -> Right result

parse :: L8.ByteString -> Either String Ini
parse bs = case runParse bs parseSections of
               Left err -> Left err
               Right sects -> Right $ Ini sects
 

---------------------
-- Pretty printing --
---------------------

-- As 'show' is intended to print haskell expression,
-- we need our own way to pretty print our data structure.
-- NOTE: 
--  1. To make the algorithm linear, ShowS is used
--  2. Space is an operator, and it has the highest priority

ppOption :: (String, String) -> ShowS
ppOption (opt, val) = \s -> concat [opt,  " = ", val, s]

ppOptionLn :: (String, String) -> ShowS
ppOptionLn optVal = ppOption optVal . (\s -> '\n' : s)

ppSection :: Section -> ShowS
ppSection sect = showSectHeaderLn . showOpts
    where showSectHeader :: ShowS
          showSectHeader = \s -> concat ["[", name sect, "]", s]

          showSectHeaderLn :: ShowS
          showSectHeaderLn = showSectHeader . (\s -> '\n' : s)

          showOpts :: ShowS
          showOpts = foldr (\ss acc -> ss . acc)
                           id
                           (map ppOptionLn $ optionValues sect)

ppSectionLn :: Section -> ShowS
ppSectionLn sect = ppSection sect . (\s -> '\n' : s)

ppIni :: Ini -> ShowS
ppIni ini = foldr (\ss acc -> ss . acc)
                  id
                  (map ppSectionLn $ sections ini)

prettyPrint :: Ini -> String
prettyPrint ini = ppIni ini ""

-- FIXME: Study how to isolate functions in this module, and only expose
--        one function that parsing the whole ini file.
--        And only necessary data types are exposed also.
-- FIXME: Study how to implement show and read properly from
--        https://www.haskell.org/tutorial/stdclasses.html
