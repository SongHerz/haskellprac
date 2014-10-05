module Main where

import Test.HUnit
import qualified RNode_Test
import qualified GlobImpl_Test

main = runTestTT $ TestList [RNode_Test.tests, GlobImpl_Test.tests]
