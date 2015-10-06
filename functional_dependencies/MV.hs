-- Vector and Matrix examples on Functional Dependencies.
-- https://wiki.haskell.org/Functional_dependencies
-- This is just an example to show functional dependencies,
-- NOT REAL matrix operations.

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Prelude hiding ((*))

data Vector = Vector Int Int
            deriving (Eq, Show)

data Matrix = Matrix Vector Vector
            deriving (Eq, Show)


class Mult a b c | a b -> c where
    (*) :: a -> b -> c

instance Mult Matrix Matrix Matrix where
    (*) m0 m1 = Matrix (Vector 0 0) (Vector 2 2)


instance Mult Matrix Vector Vector where
    (*) m0 v0 = Vector 0 0


aMatrix = Matrix (Vector 0 0) (Vector 1 1)

m1 = aMatrix
m2 = aMatrix
m3 = aMatrix

{-
- WITHOUT FUNCTIONAL DEPENDENCY,
-
- m4 = (m1 * m2) * m3
- does not compile, because:
-
- 1. the return type of 'm1 * m2' cannot be inferred without functional
- dependency.
- 2. the return type of '(m1 * m2) * m3' cannot be inferred without
- functional dependency.
-
- To make the expression compile, the return type should be explicitly
- given as below.
-}
m4 = ((m1 * m2) :: Matrix) * m3 :: Matrix

 {-
 - WITH FUNCTIONAL DEPENDENCY
 -
 - m5 = (m1 * m2) * m3
 -
 - compiles.
 -}
m5 = (m1 * m2) * m3
