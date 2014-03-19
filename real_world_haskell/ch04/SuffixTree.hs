suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _          = []

-- Without as pattern, we can not share the matched list.
-- The "Real World Haskell" says this will allocate a new node.
-- I do not know how haskell works underlying.
noAsPattern :: [a] -> [[a]]
noAsPattern (x:xs) = (x:xs) : noAsPattern xs
noAsPattern _      = []
