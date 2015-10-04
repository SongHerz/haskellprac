import Prelude hiding (odd, even)

data Nat = Zero | Succ Nat

three = Succ (Succ (Succ Zero))

even Zero = True
even (Succ n) = odd n

odd Zero = False
odd (Succ n) = even n
