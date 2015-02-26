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
    "# This is a comment" parseComment (Right $ "# This is a comment")

commentTestNoCmt = testTemplate "No comment"
    "no comment" parseComment (Right "")

sectionHeaderTest = testTemplate "Parse section header"
    "[  section ]" parseSectionHeader (Right $ "section")

sectionHeaderTestNoSect = testTemplate "No section header"
    "no section" parseSectionHeader (Right $ "")

sectionHeaderTestEmptySect = testTemplate "Empty section header"
    "[ ]" parseSectionHeader (Left $ "Error: Empty section header, offset: 3")

testCases = TestLabel "Ini unit tests" $ TestList [
        charTest
      , charTestExhausted

      , commentTest
      , commentTestNoCmt
      , sectionHeaderTest
      , sectionHeaderTestNoSect
      , sectionHeaderTestEmptySect
      ]

main = runTestTT testCases
