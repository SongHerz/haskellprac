import Parse
import Test.HUnit

import Data.ByteString.Lazy.Char8 as L8

testTemplate :: (Eq a, Show a) => String -> String -> Parse a -> Either String a -> Test
testTemplate desc iniFrag parser expectedVal = 
    TestCase $ assertEqual desc expectedVal actualVal
    where actualVal = runParse (L8.pack iniFrag) parser


charTest = testTemplate "Parse a char"
    "a" parseChar (Right 'a')

charTestExhausted = testTemplate "Parse char exhausted"
    "" parseChar (Left "Error: No more input, offset: 0")

commentTest = testTemplate "Parse a comment"
    "# This is a comment" parseComment (Right $ Just "# This is a comment")

commentTestNoCmt = testTemplate "No comment"
    "no comment" parseComment (Right Nothing)

testCases = TestLabel "Ini unit tests" $ TestList [
      charTest,
      charTestExhausted,
      commentTest,
      commentTestNoCmt]

main = runTestTT testCases
