import Parse
import Test.HUnit

import qualified Data.List as List
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
    "# This is a comment" parseComment (Right "# This is a comment")

commentTestNoCmt = testTemplate "No comment"
    "no comment" parseComment (Right "")

commentsTest1 = testTemplate "One comment"
    "# comment1" parseComments (Right ["# comment1"])

twoComments = ["# comment1", "#comment2"]
twoCommentsWithEmptyLines = List.intersperse " " twoComments
commentsTest2 = testTemplate "Two comment"
    (List.unlines twoCommentsWithEmptyLines)
    parseComments (Right twoComments)

sectionHeaderTest = testTemplate "Parse section header"
    "[  section ]" parseSectionHeader (Right $ "section")

sectionHeaderTestNoSect = testTemplate "No section header"
    "no section" parseSectionHeader (Left $ "Error: Invalid section header, offset: 0")

sectionHeaderTestEmptySect = testTemplate "Empty section header"
    "[ ]" parseSectionHeader (Left $ "Error: Empty section header, offset: 3")

optionTest = testTemplate "Option"
    "opt = somevalue  " parseOption (Right $ Just ("opt", "somevalue"))

optionTestEmptyValue = testTemplate "Option empty value"
    "opt =" parseOption (Right $ Just ("opt", ""))

optionTestNoAssign = testTemplate "Option no '='"
    "opt somevalue  " parseOption (Left "Error: Invalid option, due to no '=' found, offset: 4")

optionTestNoOpt = testTemplate "No option"
    "  " parseOption (Right $ Nothing)

options = ["opt0 = val0", "opt1 = val1"]
optionsStr = List.unlines $ "# comment" `List.intersperse` options
multiOptionsTest = testTemplate "Options"
    optionsStr parseOptions (Right [("opt0","val0"),("opt1","val1")])

aSection = [
           "[Section] # Comment after section",
           "# Comment inner section",
           "opt0 = val0",
           "# Comment inner section",
           "opt1 = val1",
           "# Comment after section"
           ]
sectionTest = testTemplate "A section"
    (List.unlines aSection) parseSection (Right $ Section "Section" [("opt0","val0"),("opt1","val1")])

sects = ["# Comment before sect",
           "",
           "[Section] # Comment after section",
           "# Comment inner section",
           "opt0 = val0",
           "# Comment inner section",
           "opt1 = val1",
           "# Comment after section",
           "",
           "[Section2]",
           "opt0 = val00",
           "opt1 = val11",
           "opt2 = val22"
           ]

multiSectionsTest = testTemplate "Multi sections"
    (List.unlines sects) parseSections
    (Right $ [
                 Section "Section" [("opt0","val0"), ("opt1","val1")]
               , Section "Section2" [("opt0", "val00"), ("opt1", "val11"), ("opt2", "val22")]
             ])


testCases = TestLabel "Ini unit tests" $ TestList [
        charTest
      , charTestExhausted

      , commentTest
      , commentTestNoCmt
      , commentsTest1
      , commentsTest2
      , sectionHeaderTest
      , sectionHeaderTestNoSect
      , sectionHeaderTestEmptySect
      , optionTest
      , optionTestEmptyValue
      , optionTestNoAssign
      , optionTestNoOpt
      , multiOptionsTest

      , sectionTest
      , multiSectionsTest
      ]

main = runTestTT testCases
