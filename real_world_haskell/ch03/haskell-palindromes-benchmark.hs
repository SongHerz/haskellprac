-- This is from the comment of stephane richard 2011-02-17 on page
-- http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html
-- on exercise 5.
-- This source is from https://gist.github.com/831449
-- And from the benchmark, the reverse version from Chandran Doray is the best.
-- It is much more better than others.
--
import Criterion
import Criterion.Main (defaultMain, bench)

-- Chris Forno 2008-09-03
isPalindromeLastInit :: (Eq a) => [a] -> Bool
isPalindromeLastInit [] = False
isPalindromeLastInit (x:[]) = True
isPalindromeLastInit (x:y:[]) = x == y
isPalindromeLastInit (x:xs) = x == last xs && isPalindromeLastInit (init xs)

-- Arlen Cuss 2008-12-26
myIsPalindrome [] = True
myIsPalindrome a  | head a /= last a = False
myIsPalindrome (x:xs) = myIsPalindrome (take (length xs - 1) xs)

-- Chandran Doray 2009-01-16
isPalindromeReverse :: Eq a => [a] -> Bool
isPalindromeReverse []    = True
isPalindromeReverse [x]   = True
isPalindromeReverse a     = a == reverse a

-- Joel Neely 2009-04-07
isPalindromeRevtake vs = (drop n vs) == revtake m vs
            where len = length vs
                  m   = quot len 2
                  n   = len - m
                  revtake1 0 vs rs = rs
                  revtake1 n (v:vs) rs = revtake1 (n-1) vs (v:rs)
                  revtake1 n [] rs = rs
                  revtake m vs = revtake1 m vs []

-- Blake Hegerle 2009-12-04
isPalindromeRaw l = p l [] 0
                  where
                    p (x:xs) r n      = p (xs) (x:r) (n + 1);
                    p [] r n          = q l r (n / 2);
                    q l r n | n < 1   = True;
                    q (x:xs) (y:ys) n = x == y && (q xs ys (n - 1))

testAll f = map f [dataA, dataB, dataC]

--
dataLength = 9999
dataA      = [1..(dataLength/2)] ++ reverse [1..(dataLength/2)]
dataB      = [1..dataLength]
dataC      = [1..(dataLength/3)] ++ [1..(dataLength/3)]  ++ reverse [1..(dataLength/3)]

main = defaultMain [

        bgroup "palindrome" [
             bench "Reverse"         $ whnf isPalindromeReverse  dataA
           , bench "Last Init"       $ whnf isPalindromeLastInit dataA
           , bench "Take Length"     $ whnf myIsPalindrome       dataA
           , bench "Revised Take"    $ whnf isPalindromeRevtake  dataA
           , bench "Raw"             $ whnf isPalindromeRaw      dataA
           ]

    ,   bgroup "no" [
             bench "Reverse"         $ whnf isPalindromeReverse  dataB
           , bench "Last Init"       $ whnf isPalindromeLastInit dataB
           , bench "Take Length"     $ whnf myIsPalindrome       dataB
           , bench "Revised Take"    $ whnf isPalindromeRevtake  dataB
           , bench "Raw"             $ whnf isPalindromeRaw      dataB
           ]

    ,   bgroup "fake" [
             bench "Reverse"         $ whnf isPalindromeReverse  dataC
           , bench "Last Init"       $ whnf isPalindromeLastInit dataC
           , bench "Take Length"     $ whnf myIsPalindrome       dataC
           , bench "Revised Take"    $ whnf isPalindromeRevtake  dataC
           , bench "Raw"             $ whnf isPalindromeRaw      dataC
           ]

    ]
