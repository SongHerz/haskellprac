module GlobImpl_Test (tests) where

import GlobImpl
import Test.HUnit

-- [case name, string to be matched, pattern, expected result]
tests' :: [(String, String, String, Bool)]
tests' = [
    ("Empty matches empty", "", "", True),
    ("Empty not matches non-empty", "", "a", False),
    ("Non-empty not matches empty", "a", "", False),
    ("Plain text match", "abc", "abc", True),
    ("Star only match 0", "", "*", True),
    ("Star only match 1", "a", "*", True),
    ("Star only match 2", "bc", "*", True),
    ("Two stars match 0", "", "**", True),
    ("Two stars match 1", "a", "**", True),
    ("Two stars match 2", "bc", "**", True),
    ("Two stars match 3", "abc", "**", True),
    ("Question only match", "b", "?", True),
    ("Empty not match question", "", "?", False),
    ("Inclusive group match 0", "a", "[a-c]", True),
    ("Inclusive group match 1", "z", "[a-c]", False),
    ("Empty not match group", "", "[a-c]", False),
    ("Exclusive group match 0", "a", "[!a-c]", False),
    ("Exclusive group match 1", "z", "[!a-c]", True),
    ("Component match 0", "abcdef.ext", "ab[c]*f.[!A-Z]??", True),
    ("Component match 1", "abcdef.ext", "ab[c]*f.[!e]??", False)

    ]


createTest :: (String, String, String, Bool) -> Test
createTest (name, string, pattern, expectResult) =
    TestLabel name aTest
    where aTest = TestCase $
                  assertEqual "" expectResult (matchesGlob string pattern)


tests = TestLabel "GlobImpl Tests" $ TestList $ map createTest tests'
