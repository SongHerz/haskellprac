module Tests where

import Test.QuickCheck
import EAN13
import Data.Array (listArray)

encodeDigitsToPixmap :: [Int] -> Pixmap

prop_closeloop :: Bool
prop_closeloop = True
