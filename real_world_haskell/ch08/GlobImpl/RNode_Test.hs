module RNode_Test (tests) where

import RNode
import Test.HUnit


-- [(case name, case message prefix, pattern, [RNode])]
tests' :: [(String, String, String, [RNode])]
tests' = [
    ("Empty pattern", "", "", []),
    ("Star", "", "*", [RStar]),
    ("Question mark", "", "?", [RQuestion]),
    ("One char", "", "a", [RChar 'a']),
    ("Two chars", "", "ab", [RChar 'a', RChar 'b']),
    ("Three chars", "", "abc", [RChar 'a', RChar 'b', RChar 'c']),
    ("Star, then char", "", "*a", [RStar, RChar 'a']),
    ("Char, then star", "", "a*", [RChar 'a', RStar]),
    ("Question, then char", "", "?a", [RQuestion, RChar 'a']),
    ("Char, then question", "", "a?", [RChar 'a', RQuestion]),
    ("Star, then question", "", "*?", [RStar, RQuestion]),
    ("Question, then star", "", "?*", [RQuestion, RStar]),
    ("Escape sequences", "", "\\?\\*\\[\\a\\\\", [RChar '?', RChar '*', RChar '[', RChar 'a', RChar '\\']),
    ("Empty inclusive character class", "", "[]", [RClass {inclusive = True, chars = ""}]),
    ("Inclusive character class with only '['", "", "[[]", [RClass {inclusive = True, chars = "["}]),
    ("Inclusive character class with only ']'", "", "[\\]]", [RClass {inclusive = True, chars = "]"}]),
    ("Inclusive character class with only '!'", "", "[\\!]", [RClass {inclusive = True, chars = "!"}]),
    ("Inclusive character class with only '\\'", "", "[\\\\]", [RClass {inclusive = True, chars = "\\"}]),
    ("Inclusive character class with ']' and '!' 0", "", "[\\]\\!]", [RClass {inclusive = True, chars = "]!"}]),
    ("Inclusive character class with ']' and '!' 1", "", "[a\\]!]", [RClass {inclusive = True, chars = "a]!"}]),
    ("Empty exclusive character class", "", "[!]", [RClass {inclusive = False, chars = ""}])

    ]

createTest :: (String, String, String, [RNode]) -> Test
createTest (name, message, pattern, expectResult) = 
    TestLabel name aTest
    where aTest = TestCase $
                  assertEqual message expectResult (preProcess pattern)

     

tests = TestLabel "RNode Tests" $ TestList $ map createTest tests'

