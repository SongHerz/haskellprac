-- | Operators
data Op = Plus | Minus | Mul | Div | Pow
        deriving (Eq, Show)

-- | Core symbolic manipulation type
data SymbolicManip a = 
          Number a
        | Arith Op (SymbolicManip a) (SymbolicManip a)
        deriving (Eq, Show)

-- SymbolicManip will be an instance of Num.
instance Num a => Num (SymbolicManip a) where
    a + b = Arith Plus a b
    a - b = Arith Minus a b
    a * b = Arith Mul a b
    negate a = Arith Mul (Number (-1)) a
    abs _ = error "abs unimplemented"
    signum _ = error "signum unimplemented"
    fromInteger i = Number (fromInteger i)
