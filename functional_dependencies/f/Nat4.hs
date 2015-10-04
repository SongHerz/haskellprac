{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

import Prelude hiding (odd, even)

data Zero

data Succ n

type Three = Succ (Succ (Succ Zero))

data True
        
data False

class Even n b | n -> b where
    even :: n -> b

class Odd n b | n -> b where
    odd :: n -> b

instance Even Zero True

instance Odd n b => Even (Succ n) b

instance Odd Zero False

instance Even n b => Odd (Succ n) b


class Add a b c | a b -> c where
    add :: a -> b -> c

instance Add Zero b b 

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


class Mul a b c | a b -> c where
    mul :: a -> b -> c

instance Mul Zero b Zero

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
