{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

import Prelude hiding (odd, even, pred)

data Zero

data Succ n

type Three = Succ (Succ (Succ Zero))

data True
        
data False

class Even n b | n -> b where even :: n -> b

class Odd n b | n -> b where odd :: n -> b

instance Even Zero True

instance Odd n b => Even (Succ n) b

instance Odd Zero False

instance Even n b => Odd (Succ n) b

{-
WITH functional dependencies, the complete type can be inferred.

*Main> :type even (undefined::Three)
even (undefined::Three) :: False

*Main> :type odd (undefined::Three)
odd (undefined::Three) :: True
-}


class Add a b c | a b -> c where add :: a -> b -> c

instance Add Zero b b 

{- a + b = c => (a + 1) + b = c + 1 -}
instance Add a b c => Add (Succ a) b (Succ c)

{-
*Main> :type add (u::Three) (u::Three)
add (u::Three) (u::Three)
  :: Succ (Succ (Succ (Succ (Succ (Succ Zero)))))

Derivation:
add :: Three -> Three -> c
belongs to instance:
Add Three Three c = Add (Succ Two) Three (Succ c1)

Write the instance and its constrain:
instance Add Two Three c1 => Add (Succ Two) Three (Succ c1)

Write the instance of the above constain,
and the instance of the instance of the above constrain:
Add Two Three c1 = Add (Succ One) Three (Succ c2)
instance Add One Three c2 => Add (Succ One) Three (Succ c2)

And so on:
Add One Three c2 => Add (Succ Zero) Three (Succ c3)
instance Add Zero Three c3 => Add (Succ Zero) Three (Succ c3)

Add Zero Three c3 = Add Zero Three Three
==> c3 = Three

Because (from above type equations):
c2 = Succ c3
c1 = Succ c2
c = Succ c1

c = Succ (Succ (Succ Three)) = Succ (Succ (Succ (Succ (Succ (Succ Zero)))))
-}


class Mul a b c | a b -> c where mul :: a -> b -> c

instance Mul Zero b Zero

{- a * b = c, b + c = d => (a + 1) * b = a * b + b = c + b = d -}
instance (Mul a b c, Add b c d) => Mul (Succ a) b d

u = undefined

{-*Main> :type mul (u::Three) (u::Three)
mul (u::Three) (u::Three)
  :: Succ
       (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))

Derivation:
mul :: Three -> Three -> d
belongs to instance:
Mul (Succ Two) b d

Write the instance and its constrain:
(Mul Two Three c, Add Three c d) => Mul (Succ Two) Three d

Derive constrains of Mul first:

Mul Two Three c = Mul (Succ One) Three c
Write the above instance with its constrain:
(Mul One Three c1, Add Three c1 c) => Mul (Succ One) Three c

And so on:
Mul One Three c1 = Mul (Succ Zero) Three c1
(Mul Zero Three c2, Add Three c2 c1) => Mul (Succ Zero) Three c1

Mul Zero Three c2 = Mul Zero Three Zero

==> c2 = Zero

Now, let us derive 'Add Three c2 c1'
Add Three c2 c1 = Add (Succ Two) c2 (Succ c11)
Add Two c2 c11 => Add (Succ Two) c2 (Succ c11)

And so on:
Add Two c2 c11 = Add (Succ One) c2 (Succ c12)
Add One c2 c12 => Add (Succ One) c2 (Succ c12)

Add One c2 c12 = Add (Succ Zero) c2 (Succ c13)
Add Zero c2 c13 => Add (Succ Zero) c2 (Succ c13)

=> c2 = c13

Because:
c1 = Succ c11
c11 = Succ c12
c12 = Succ c13

==> c1 = Succ (Succ (Succ c13)) = Succ (Succ (Succ c2))

The same:
c = Succ (Succ (Succ c1))
d = Succ (Succ (Succ c))

=> :type mul (u::Three) (u::Three)
=   Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ c2))))))))
=   Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))
-}


-- ---------------------
-- Power Static Version
-- ---------------------
type One = Succ Zero

class Pow a b c | a b -> c where pow :: a -> b -> c

instance Pow a Zero One

{- a^b = c, a * c = d => a^(b + 1) = (a^b) * a = c * a = d -}
instance (Pow a b c, Mul a c d) => Pow a (Succ b) d

{-
*Main> :type pow (u::Succ (Succ Zero)) (u::Zero)
pow (u::Succ (Succ Zero)) (u::Zero) :: One

*Main> :type pow (u::Succ One) (u::One)
pow (u::Succ One) (u::One) :: Succ (Succ Zero)

*Main> :type pow (u::Succ One) (u::Succ One)
pow (u::Succ One) (u::Succ One) :: Succ (Succ (Succ (Succ Zero)))
-}


-- -----------------------------------
-- Power Static/Dynamic Mixed Version
-- -----------------------------------

{- Pred means predecessor -}
class Pred a b | a -> b where pred :: a -> b

instance Pred (Succ n) n


class Power n where
    power :: Int -> n -> Int

instance Power Zero where
    power _ _ = 1

instance Power n => Power (Succ n) where
    power x n = x * power x (pred n)

{-
=================
power x (u::Zero)
=================
power x (u::Zero)
belongs to instance Power Zero.
Thus, power x (u::Zero) = 1

======================
power x (u::Succ Zero)
======================
power x (u::Succ Zero)
belongs to instance Power n => Power (Succ n)
Thus, power x (u::Succ Zero) = x * power x (pred (u::Succ Zero))

pred (u::Succ Zero)
belongs to instance (Succ Zero) Zero.
Thus, the type of (pred (u::Succ Zero)) is Zero. 
And (power x (pred (u::Succ Zero))) belongs to instance Power Zero => Power (Succ Zero)
And (power x (pred (u::Succ Zero)) = 1)
And power x (u::Succ Zero) = x * 1 = x

=====================
power x (u::Succ One)
=====================
power x (u::Succ One)
belongs to instance Power n => Power (Succ n)
Thus, power x (u::Succ One) = x * power x (pred (u::Succ One))
The type of (pred (u::Succ One)) is One,
and (power x (pred (u::Succ One))) belongs instance Power n => Power (Succ n)
Thus
power x (pred (u::Succ One)) = x * power x (pred (pred (u::Succ One)))

As type of (pred (u::Succ One)) is One/Succ Zero,
type of (pred (pred (u::Succ One))) is type of (pred some_val::(Succ Zero)).

Type of (pred some_val::(Succ Zero)) is Zero,
and (power x (pred some_val::(Succ Zero))) belongs to instance Power Zero => Power (Succ Zero)
And (power x (pred some_val::(Succ Zero)) = 1)
And (power x (pred (pred (u::Succ One))) = 1)
And (power x (pred (u::Succ One)) = x * power x (pred (pred (u::Succ One)) = x)
And (power x (u::Succ One) = x * power x (pred (u::Succ One)) = x * x)

==========
Conclution
==========
power x (u::Natural) = x ^ Natural
-}

{-
As shown above (mul (u::Three) (u::Three) = Nine)

*Main> power 2 (mul (u::Three) (u::Three))
512
-}
