module FindIdentifier_Test where

import FindIdentifier ( findIdentifier)
import Test.HUnit
import Control.OldException

borderCases = TestLabel "Border test cases" $ TestList [testEmpty, testNegCursor, testComment]

testEmpty = TestCase $ assertEqual
    "Should get Nothing from an empty string" Nothing ( findIdentifier "" (1,1))

testNegCursor = TestCase $ do 
    handleJust errorCalls (\_ -> return ()) performCall where
        performCall = do
            evaluate( findIdentifier "a" (-1,-1))
            assertFailure "Cursor position (-1,-1) must throw an error"

testComment = TestCase $ assertEqual
    "Should get Nothing on comment" Nothing ( findIdentifier "-- a" (1,3))


simpleCases = TestLabel "Simple, but serious cases" $
              TestList [ testMinimal, testData]

testMinimal = TestCase $ assertEqual
    "Minimal program" (Just "main") ( findIdentifier "main = print 42" (1,2))
testData = TestCase $ do
    let code = "main = print 42\ndata Bli = Bla | Blubb"
    assertEqual
        "Data declaration - on identifier"
        (Just "Bli")
        (findIdentifier code (2,7))
    assertEqual
        "Data declaration - before identifier"
        (Just "Bli")
        (findIdentifier code (2,6))
    assertEqual
        "Data declaration - after identifier"
        (Just "Bli")
        (findIdentifier code (2,9))


main = runTestTT $ TestList [ borderCases, simpleCases]
