import Data.List (tails)

suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _          = []

-- Without as pattern, we can not share the matched list.
-- The "Real World Haskell" says this will allocate a new node.
-- I do not know how haskell works underlying.
noAsPattern :: [a] -> [[a]]
noAsPattern (x:xs) = (x:xs) : noAsPattern xs
noAsPattern _      = []

suffixes2 :: [a] -> [[a]]
suffixes2 xs = init (tails xs)

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

suffixes4 = compose init tails

suffixes5 = init . tails
