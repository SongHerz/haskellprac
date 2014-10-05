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
    -- Inclusive character class cases
    ("Empty inclusive character class", "", "[]", [RClass {inclusive = True, chars = ""}]),
    ("Inclusive character class with only '['", "", "[[]", [RClass {inclusive = True, chars = "["}]),
    ("Inclusive character class with only ']'", "", "[\\]]", [RClass {inclusive = True, chars = "]"}]),
    ("Inclusive character class with only '!'", "", "[\\!]", [RClass {inclusive = True, chars = "!"}]),
    ("Inclusive character class with only '\\'", "", "[\\\\]", [RClass {inclusive = True, chars = "\\"}]),
    ("Inclusive character class with ']' and '!' 0", "", "[\\]\\!]", [RClass {inclusive = True, chars = "]!"}]),
    ("Inclusive character class with ']' and '!' 1", "", "[a\\]!]", [RClass {inclusive = True, chars = "a]!"}]),
    ("Inclusive character class with normal chars", "", "[abc]", [RClass {inclusive = True, chars = "abc"}]),
    -- Exclusive character class cases
    ("Empty exclusive character class", "", "[!]", [RClass {inclusive = False, chars = ""}]),
    ("Exclusive character class with only '['", "", "[![]", [RClass {inclusive = False, chars = "["}]),
    ("Exclusive character class with only ']'", "", "[!\\]]", [RClass {inclusive = False, chars = "]"}]),
    ("Exclusive character class with only '!'", "", "[!!]", [RClass {inclusive = False, chars = "!"}]),
    ("Exclusive character class with only '\\'", "", "[!\\\\]", [RClass {inclusive = False, chars = "\\"}]),
    ("Exclusive character class with ']' and '!' 0", "", "[!\\]\\!]", [RClass {inclusive = False, chars = "]!"}]),
    ("Exclusive character class with ']' and '!' 1", "", "[!a\\]!]", [RClass {inclusive = False, chars = "a]!"}]),
    ("Exclusive character class with normal chars", "", "[!abc]", [RClass {inclusive = False, chars = "abc"}]),
    -- Character range support
    ("Inclusive with '-' 0", "", "[-]", [RClass {inclusive = True, chars = "-"}]),
    ("Inclusive with '-' 1", "", "[-a]", [RClass {inclusive = True, chars = "-a"}]),
    ("Inclusive with '-' 2", "", "[a\\-c]", [RClass {inclusive = True, chars = "a-c"}]),
    ("Inclusive with '-' 3", "", "[-z]", [RClass {inclusive = True, chars = "-z"}]),
    ("Inclusive with '-' 4", "", "[-az]", [RClass {inclusive = True, chars = "-az"}]),
    ("Inclusive with '-' 5", "", "[a\\-cz]", [RClass {inclusive = True, chars = "a-cz"}]),
    ("Inclusive normal range 0", "", "[a-c]", [RClass {inclusive = True, chars = "abc"}]),
    ("Inclusive normal range 1", "", "[\\--0]", [RClass {inclusive = True, chars = "-./0"}]),
    ("Inclusive normal range 2", "", "[\\a-\\c]", [RClass {inclusive = True, chars = "abc"}]),
    ("Inclusive normal range 3", "", "[\\a-c]", [RClass {inclusive = True, chars = "abc"}]),
    ("Inclusive normal range 4", "", "[a-\\c]", [RClass {inclusive = True, chars = "abc"}]),
    ("Inclusive normal range 5", "", "[a-cz]", [RClass {inclusive = True, chars = "abcz"}]),
    ("Inclusive normal range 6", "", "[\\--0z]", [RClass {inclusive = True, chars = "-./0z"}]),
    ("Inclusive normal range 7", "", "[\\a-\\cz]", [RClass {inclusive = True, chars = "abcz"}]),
    ("Inclusive normal range 8", "", "[\\a-cz]", [RClass {inclusive = True, chars = "abcz"}]),
    ("Inclusive normal range 9", "", "[a-\\cz]", [RClass {inclusive = True, chars = "abcz"}]),
    ("Inclusive range after range 0", "", "[a-\\cx-z]", [RClass {inclusive = True, chars = "abcxyz"}]),
    ("Inclusive range , char, then range", "", "[a-\\cfx-z]", [RClass {inclusive = True, chars = "abcfxyz"}]),
    ("Exclusive with '-' 0", "", "[!-]", [RClass {inclusive = False, chars = "-"}])

    ]

createTest :: (String, String, String, [RNode]) -> Test
createTest (name, message, pattern, expectResult) = 
    TestLabel name aTest
    where aTest = TestCase $
                  assertEqual message expectResult (preProcess pattern)

     

tests = TestLabel "RNode Tests" $ TestList $ map createTest tests'

