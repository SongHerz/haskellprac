import Parse
import Test.HUnit

testCases = TestLabel "Ini unit tests" $ TestList []

main = runTestTT testCases
