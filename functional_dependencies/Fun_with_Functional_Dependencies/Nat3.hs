{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

import Prelude hiding (odd, even)

data Zero

data Succ n

type Three = Succ (Succ (Succ Zero))

data True
        
data False

class Even n b where
    even :: n -> b

class Odd n b where
    odd :: n -> b

instance Even Zero True

instance Odd n b => Even (Succ n) b

instance Odd Zero False

instance Even n b => Odd (Succ n) b

{-
As functional dependency has not been specified here,
the type checker cannot determine the result type completely.

*Main> :type odd (undefined::Three)
odd (undefined::Three) :: Even Zero b => b


WITHOUT functional dependency, True/False should be specified
at type checking time.
When type checking passes, the predicate is right.
Otherwise, the predicate is wrong.

*Main> :type odd (undefined::Three) :: True
odd (undefined::Three) :: True :: True

*Main> :type odd (undefined::Three) :: False

<interactive>:1:1:
    No instance for (Even Zero False) arising from a use of ‘odd’
    In the expression: odd (undefined :: Three) :: False

*Main> :type even (undefined::Three) :: False
even (undefined::Three) :: False :: False

*Main> :type even (undefined::Three) :: True

<interactive>:1:1:
    No instance for (Odd Zero True) arising from a use of ‘even’
    In the expression: even (undefined :: Three) :: True
-}
