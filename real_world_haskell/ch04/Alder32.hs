import Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))

base = 65521

alder32 xs = helper 1 0 xs
             where helper a b (x:xs) = let a' = (a + (ord x .&. 0xff)) `mod` base
                                           b' = (a' + b) `mod` base
                                       in helper a' b' xs
                   helper a b _       = (b `shiftL` 16) .|. a

alder32_try2 xs = helper (1,0) xs
             where helper (a,b) (x:xs) = let a' = (a + (ord x .&. 0xff)) `mod` base
                                             b' = (a' + b) `mod` base
                                         in helper (a', b') xs
                   helper (a,b) _      = (b `shiftL` 16) .|. a

-- let c = a + (ord x .&. 0xff),
-- Form 1
-- a' = c % base
-- b' = (a' + b) % base
--    = ( c % base + b) % base
--    = c % base % base + b % base
--    = c % base + b % base
--
-- So the above expressions can be expressed as
-- Form 2
-- a' = c
-- b' = (a' + b) % base
--    = c % base + b % base
-- which is actually the same with form 1.

-- Notice the difference in this function,
-- and it is explained by the above comment.
alder32_foldl xs = let (a,b) = foldl step (1,0) xs
                   in (b `shiftL` 16) .|. a
    where step (a,b) x = let a' = (a + (ord x .&. 0xff))
                             b' = (a' + b) `mod` base
                         in (a' `mod` base, b')

