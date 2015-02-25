{-# LANGUAGE ScopedTypeVariables #-}
module Parse (parse) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Int (Int64)
import Data.Word (Word8)
import Data.Char (chr, isDigit, isSpace)
import Control.Applicative ((<$>))

import Greymap (Greymap(..))

data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64
    } deriving (Show)

newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
    }

identity :: a -> Parse a
identity a = Parse $ \s -> Right (a, s)

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset =
    initState { offset = newOffset }

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

bail :: String -> Parse a
bail err = Parse $ \s -> Left $
           "byte offet " ++ show (offset s) ++ ": " ++ err

(==>) :: forall a b. Parse a -> (a -> Parse b) -> Parse b
firstParser ==> parseFunc = Parse chainedParser
    where chainedParser :: ParseState -> Either String (b, ParseState)
          chainedParser initState =
            case runParse firstParser initState of
                Left errMessage ->
                    Left errMessage
                Right (firstResult, newState) ->
                    runParse (parseFunc firstResult) newState

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ -> f

instance Functor Parse where
    -- fmap :: (Functor Parse) => (a -> b) -> Parse a -> Parse b
    fmap f parser = parser ==> \result ->
                    identity (f result)


parseByte :: Parse Word8
parseByte =
    getState ==> \initState ->
    case L.uncons (string initState) of
        Nothing ->
            bail "no more input"
        Just (byte, reminder) ->
            putState newState ==> \_ ->
            identity byte
            where newState = initState { string = reminder,
                                         offset = newOffset }
                  newOffset = offset initState + 1

whites :: String
whites = " \t\r\n"

lineTerms :: String
lineTerms = "\r\n"

notWhite :: Char -> Bool
notWhite = (`notElem` whites)

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
               if mp == Just True
               then parseByte ==> \b ->
                    (b:) <$> parseWhile p
               else identity []


parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

skipComment :: Parse ()
skipComment = peekChar ==> \mc -> 
              case mc of
                  Just '#' -> parseWhileWith w2c (`notElem` lineTerms) ==>& identity ()
                  _        -> identity ()

-- FIXME: This function cannot skip consecutive comments for now.
--
--        E .g.
--        # Some comment
--        ... Arbitrary spaces and comments inside
--        # another comment
--        Add this feature when implementing ascii pgm parsing.
skipSpcCmt :: Parse ()
skipSpcCmt = skipSpaces ==>& skipComment ==>& skipSpaces

assert :: Bool -> String -> Parse ()
assert True _    = identity ()
assert False err = bail err

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
           if null digits
           then bail "no more input"
           else let n = read digits
                in if n < 0
                   then bail "integer overflow"
                   else identity n

parseBytes :: Int -> Parse L.ByteString
parseBytes n =
    getState ==> \st ->
    let n' = fromIntegral n
        (h, t) = L.splitAt n' (string st)
        st' = st { offset = offset st + L.length h, string = t }
    in putState st' ==>&
       assert (L.length h == n') "end of input" ==>&
       identity h

parseRawPGM :: Parse Greymap
parseRawPGM =
    parseWhileWith w2c notWhite ==> \header -> skipSpcCmt ==>&
    assert (header == "P5") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpcCmt ==>&
    parseNat ==> \height -> skipSpcCmt ==>&
    parseNat ==> \maxGrey ->
    parseByte ==>&
    parseBytes (width * height) ==> \bitmap ->
    identity (Greymap width height maxGrey bitmap)

runAParser :: Parse a -> L.ByteString -> Either String a
runAParser parser initStr =
    case runParse parser (ParseState initStr 0) of
        Left err            -> Left err
        Right (result, _)   -> Right result

parse :: L.ByteString -> Either String Greymap
parse initStr = runAParser parseRawPGM initStr
