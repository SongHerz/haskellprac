module RNode_Test (tests) where

import RNode
import Test.HUnit


tests' :: [(String, String, String, [RNode])]
tests' = [
    ("Empty Pattern", "", "", [])
    ]

createTest :: (String, String, String, [RNode]) -> Test
createTest (name, message, pattern, expectResult) = 
    TestLabel name aTest
    where aTest = TestCase $
                  assertEqual message (preProcess pattern) expectResult

     

tests = TestLabel "RNode Tests" $ TestList $ map createTest tests'

main = runTestTT tests
