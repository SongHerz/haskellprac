{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Int (Int64)
import Data.Word (Word8)
import Data.Char (chr)
import Control.Applicative ((<$>))

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


parse :: Parse a -> L.ByteString -> Either String a
parse parser initStr =
    case runParse parser (ParseState initStr 0) of
        Left err            -> Left err
        Right (result, _)   -> Right result
