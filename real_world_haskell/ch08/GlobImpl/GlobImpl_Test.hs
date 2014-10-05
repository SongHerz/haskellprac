module GlobImpl_Test (tests) where

import GlobImpl
import Test.HUnit

-- [case name, string to be matched, pattern, expected result]
tests' :: [(String, String, String, Bool)]
tests' = [
    ("Empty matches empty", "", "", True),
    ("Empty not matches non-empty", "", "a", False),
    ("Non-empty not matches empty", "a", "", False)
    ]


createTest :: (String, String, String, Bool) -> Test
createTest (name, string, pattern, expectResult) =
    TestLabel name aTest
    where aTest = TestCase $
                  assertEqual "" expectResult (matchesGlob string pattern)


tests = TestLabel "GlobImpl Tests" $ TestList $ map createTest tests'
