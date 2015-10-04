data Zero

data Succ n

type Three = Succ (Succ (Succ Zero))

class Even n where
    isEven :: n

class Odd n where
    isOdd :: n

instance Even Zero

instance Odd n => Even (Succ n)

instance Even n => Odd (Succ n)

{-
*Main> :type isEven :: Three

<interactive>:1:1:
    No instance for (Odd Zero) arising from a use of ‘isEven’
        In the expression: isEven :: Three

isEven :: Three
belongs to class Even Three

Even Three = Even (Succ Two)
Write the full instance with constrain.
Odd Two => Even (Succ Two)

The same:
Odd Two = Odd (Succ One)
Even One => Odd (Succ One)

Even One = Even (Succ Zero)
Odd Zero => Even (Succ Zero)

As there is no instance 'Odd Zero', 'isEven Three' fails.

The same procedure:
':type isOdd :: Three' success.
-}
