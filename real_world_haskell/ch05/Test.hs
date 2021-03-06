-- This is created when reading Ch11
module Test where

import Prettify

import Test.QuickCheck
import Data.List (intersperse)
import Control.Monad (liftM, liftM2)

-- This is a better version without intermediate vars.
instance Arbitrary Doc where
        arbitrary =
            oneof [ return Empty
                  , liftM Char arbitrary
                  , liftM Text arbitrary
                  , return Line
                  , liftM2 Concat arbitrary arbitrary
                  , liftM2 Union arbitrary arbitrary]

prop_empty_id x = 
    empty <> x == x
  &&
    x <> empty == x

prop_char c = char c == Char c
prop_text s = text s == if null s then Empty else Text s
prop_line = line == Line
prop_double d = double d == text (show d)

prop_hcat xs = hcat xs == glue xs
    where
        glue [] = empty
        glue (d:ds) = d <> glue ds

prop_punctuate s xs = punctuate s xs == combine (intersperse s xs)
    where
        combine [] = []
        combine [x] = [x]

        combine (x:Empty:ys) = x : combine ys
        combine (Empty:y:ys) = y : combine ys
        combine (x:y:ys) = x `Concat` y : combine ys


args = Args
    { replay = Nothing
    , maxSuccess = 200
    , maxDiscardRatio = 100
    , maxSize = 100
    , chatty = True
    }

-- TODO: Template Haskell can be used to automatically
--       run properties whose prefixes are 'prop_'.
--       See $quickCheckAll and its friends of QuickCheck module.
runTests :: Args -> IO ()
runTests args = do
        f prop_empty_id "empty id"
        f prop_char "char"
        f prop_text "text"
        f prop_line "line"
        f prop_double "double"
        f prop_hcat "hcat"
        f prop_punctuate "punctuate"
        return ()
    where
        f prop s = do
            putStrLn s
            quickCheckWithResult args prop

main :: IO ()
main = runTests args
