import Data.Function (on)
import Data.List (intercalate)
import qualified Data.Map as M

-- Operators
data Op = Plus | Minus | Mul | Div | Pow
        deriving (Eq, Show)

instance Ord Op where
    compare = compare `on` prece
        where -- precedence of operators
              prece Plus = 0
              prece Minus = 0
              prece Mul = 1
              prece Div = 1
              prece Pow = 2
         

-- Core symbolic manipulation type
data SymbolicManip a =
          Number a
        | Symbol String
        | BinaryArith Op (SymbolicManip a) (SymbolicManip a)
        | UniaryArith String (SymbolicManip a)
        deriving (Eq)

instance Num a => Num (SymbolicManip a) where
    a + b = BinaryArith Plus a b
    a - b = BinaryArith Minus a b
    a * b = BinaryArith Mul a b
    negate a = BinaryArith Mul (Number (-1)) a
    abs a = UniaryArith "abs" a
    signum _ = error "signum is unimplemented"
    fromInteger i = Number (fromInteger i)

instance Fractional a => Fractional (SymbolicManip a) where
    a / b = BinaryArith Div a b
    recip a = BinaryArith Div (Number 1) a
    fromRational r = Number (fromRational r)

instance Floating a => Floating (SymbolicManip a) where
    pi = Symbol "pi"
    exp a = UniaryArith "exp" a
    sqrt a = UniaryArith "sqrt" a
    log a = UniaryArith "log" a
    a ** b = BinaryArith Pow a b
    sin a = UniaryArith "sin" a
    tan a = UniaryArith "tan" a
    cos a = UniaryArith "cos" a
    asin a = UniaryArith "asin" a
    atan a = UniaryArith "atan" a
    acos a = UniaryArith "acos" a
    sinh a = UniaryArith "sinh" a
    tanh a = UniaryArith "tanh" a
    cosh a = UniaryArith "cosh" a
    asinh a = UniaryArith "asinh" a
    atanh a = UniaryArith "atanh" a
    acosh a = UniaryArith "acosh" a

-- Show SymbolicManip as a String with conventional algebraic notation.
prettyShow :: (Show a, Num a) => SymbolicManip a -> String
prettyShow (Number x) = show x
prettyShow (Symbol x) = x
prettyShow (BinaryArith op a b) =
    let pa = showLhs op a
        pb = showRhs op b
        pop = op2Str op
        in pa ++ pop ++ pb
    where showLhs = showBinarySub (\pop sop -> sop < pop)
          showRhs = showBinarySub (\pop sop -> sop <= pop)

          showBinarySub :: (Show a, Num a) => (Op -> Op -> Bool) -> Op -> SymbolicManip a -> String
          showBinarySub needParen parent_op x@(BinaryArith sub_op _ _) =
              let substr = prettyShow x
                  in if needParen parent_op sub_op
                         then "(" ++ substr ++ ")"
                         else substr
          showBinarySub _ _ x = prettyShow x

prettyShow (UniaryArith opstr a) =
    opstr ++ "(" ++ prettyShow a ++ ")"

op2Str :: Op -> String
op2Str Plus = "+"
op2Str Minus = "-"
op2Str Mul = "*"
op2Str Div = "/"
op2Str Pow = "**"

-- Add parenthesis where needed.
-- This is fairly conservative and will add parenthesis when not needed in
-- some cases.
-- Haskell will figure out precedence for us while building SymbolicManip.
-- NOT USED ANY MORE.
-- simpleParen :: (Show a, Num a) => SymbolicManip a -> String
-- simpleParen x@(BinaryArith _ _ _) = "(" ++ prettyShow x ++ ")"
-- simpleParen x = prettyShow x

instance (Show a, Num a) => Show (SymbolicManip a) where
    show = prettyShow

-- Show SymbolicManip in RPN form
rpnShow :: (Show a, Num a) => SymbolicManip a ->String
rpnShow i =
    let toList (Number x) = [show x]
        toList (Symbol x) = [x]
        toList (BinaryArith op a b) = toList a ++ toList b ++ [op2Str op]
        toList (UniaryArith opstr a) = toList a ++ [opstr]
    in intercalate " " (toList i)

-- Some basic simplifications
-- FIXME: GHCI requires Eq constrain on type var a, WHY ???
--
-- [1 of 1] Compiling Main             ( num.hs, interpreted )
-- 
-- num.hs:94:26:
--     Could not deduce (Eq a) arising from the literal ‘1’
--     from the context (Num a)
--       bound by the type signature for
--                  simplify :: Num a => SymbolicManip a -> SymbolicManip a
--       at num.hs:88:13-57
--     Possible fix:
--       add (Eq a) to the context of
--         the type signature for
--           simplify :: Num a => SymbolicManip a -> SymbolicManip a
--     In the pattern: 1
--     In the pattern: Number 1
--     In the pattern: (Mul, Number 1, b)
-- Failed, modules loaded: none.
simplify :: (Num a, Eq a) => SymbolicManip a -> SymbolicManip a
simplify (BinaryArith op a b) =
    let sa = simplify a
        sb = simplify b
        in
        case (op, sa, sb) of
            (Mul, Number 1, b) -> b
            (Mul, a, Number 1) -> a
            (Mul, Number 0, _) -> Number 0
            (Mul, _, Number 0) -> Number 0
            (Div, a, Number 1) -> a
            (Plus, a, Number 0) -> a
            (Plus, Number 0, b) -> b
            (Minus, a, Number 0) -> a
            _ -> BinaryArith op sa sb
simplify (UniaryArith op a) = UniaryArith op (simplify a)
simplify x = x

-- Unit type
data Units a  = Units a (SymbolicManip a)
              deriving (Eq)

instance (Num a, Eq a) => Num (Units a) where
    (Units xa ua) + (Units xb ub)
        | ua == ub = Units (xa + xb) ua
        | otherwise = error "Mis-matched units in add or subtract"
    a - b = a + (negate b)
    (Units xa ua) * (Units xb ub) = Units (xa * xb) (ua * ub)
    negate (Units xa ua) = Units (negate xa) ua
    abs (Units xa ua) = Units (abs xa) ua
    signum (Units xa ua) = Units (signum xa) (Number 1)
    fromInteger i = Units (fromInteger i) (Number 1)

instance (Fractional a, Eq a) => Fractional (Units a) where
    (Units xa ua) / (Units xb ub) = Units (xa / xb) (ua / ub)
    recip a = 1 / a
    fromRational r = Units (fromRational r) (Number 1)

-- Use some intelligence for angle calculations: support deg and rad
instance (Floating a, Eq a) => Floating (Units a) where
        pi = Units pi (Number 1)
        exp _ = error "exp not yet implemented"
        log _ = error "log not yet implemented"
        (Units xa ua) ** (Units xb ub)
            | ub == Number 1 = Units (xa ** xb) (ua ** Number xb)
            | otherwise = error "units for RHS of ** not supported"
        sqrt (Units xa ua) = Units (sqrt xa) (sqrt ua)
        sin a = trigo sin a
        cos a = trigo cos a
        tan a = trigo tan a
        asin a = invTrigo asin a
        acos a = invTrigo acos a
        atan a = invTrigo atan a
        sinh = error "sinh not yet implemented in Units"
        cosh = error "cosh not yet implemented in Units"
        tanh = error "tanh not yet implemented in Units"
        asinh = error "asinh not yet implemented in Units"
        acosh = error "acosh not yet implemented in Units"
        atanh = error "atanh not yet implemented in Units"

trigo :: (Floating a, Eq a) => (a -> a) -> Units a -> Units a
trigo f (Units xa ua)
    | ua == Symbol "rad" = Units (f xa) (Number 1)
    | ua == Symbol "deg" = Units (f $ deg2rad xa) (Number 1)
    | otherwise = error "Units for a trigonometric functionn must be deg or rad"
    where deg2rad x = 2 * pi * x / 360

invTrigo :: (Floating a, Eq a) => (a -> a) -> Units a -> Units a
invTrigo f (Units xa ua)
    | ua == Number 1 = Units (f xa) (Symbol "rad")
    | otherwise = error "Units for an inverse trigonometric function must be unit 1"

units :: (Num z) => z -> String -> Units z
units a b = Units a (Symbol b)

dropUnits :: (Num z) => Units z -> z
dropUnits (Units x _) = x

instance (Show a, Num a, Eq a) => Show (Units a) where
    show (Units xa ua) = show xa ++ "_" ++ prettyShow (simplify ua)

-- Eval a SymbolicManip, and get value of symbols from the given map.
-- Raise error when a symbol cannot be found from the map.
eval :: (Num a, Ord a) => M.Map String a -> SymbolicManip a -> a
eval _ (Number n) = n
eval m (Symbol s) =
    case M.lookup s m of
        Just v -> v
        Nothing -> error $ "Cannot find value of symbol: " ++ show s
eval m (BinaryArith op a b) =
    let va = eval m a
        vb = eval m b
    in calc op va vb
    where calc :: Num a => Op -> a -> a -> a
          calc Plus a b = a + b
          calc Minus a b = a - b
          calc Mul a b = a * b
          calc Div _ _ = error "DIV not general for all Num instances"
          calc Pow _ _ = error "POW not general for all Num instances"
eval m (UniaryArith opstr a) =
   let v = eval m a
   in calc opstr v
   where calc :: Num a => String -> a -> a
         -- It looks only abs is general for all Num instances
         calc "abs" v = abs v
         calc opstr _ = error $ "Do not support '" ++ opstr ++ "' for all Num instances"

-- -------------------
--   CONSTANT DEMO
-- -------------------
consts = 1 + (5 - 3)*(UniaryArith "abs" (-2)) :: SymbolicManip Int
constsval = eval M.empty consts -- result is 5

-- --------------------
--   DEMO with Symbol
-- --------------------
symbolconsts = 1 + (5 - (Symbol "x"))*(UniaryArith "abs" (-2)) :: SymbolicManip Int
symbolconstsval = eval (M.singleton "x" 3) symbolconsts -- result is 5

-- --------------------
--   Errors
-- --------------------
symbolconstsval_cannotfound = eval M.empty symbolconsts -- expect exception
-- not supported binary op
unsupported_div = eval M.empty $ BinaryArith Div (Number 1) (Number 2) -- expect exception
-- not supported unary op
unsupported_sin = eval M.empty $ UniaryArith "sin" (Number 1) -- except exception
