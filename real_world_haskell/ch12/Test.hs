module Main where

import Test.QuickCheck
import Tests (prop_closeloop)


runTests :: Args -> IO ()
runTests args = do
    quickCheckWithResult args prop_closeloop
    return ()

args = Args {
      replay = Nothing
    , maxSuccess = 200
    , maxDiscardRatio = 100
    , maxSize = 100
    , chatty = True
    }

main = runTests args
