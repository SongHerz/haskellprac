-- Show behaviors of do notion.
--
robust :: [a] -> Maybe a
robust xs = do
    (_:x:_) <- Just xs
    return x


-- This should evaled to Nothing
r0 = robust [0]

-- It looks lambda functions cannot use pattern match.
-- \(_:x:_) -> ...
-- is not legal.
-- And I have to define a function, and then use that function
notRobust :: [a] -> Maybe a
notRobust xs =
    let f (_:x:_) = return x
    in (Just xs) >>= f

-- This should raise an exception
r1 = notRobust [0]


letInDo :: [Int]
letInDo = do
    let val1 = 3
        val2 = 4
    return $ val1 + val2

translatedLetInDo :: [Int]
translatedLetInDo =
    let val1 = 3
        val2 = 4
    in do 
          return $ val1 + val2

letInDo' :: [Int]
letInDo' = do
    let val1 = 3
    let val2 = 4
    return $ val1 + val2

{- This does not compile, as 'val2 = 4' is not well indented.
letInDo'' :: [Int]
letInDo'' = do
    let val1 = 3
    val2 = 4
    return $ val1 + val2
-}
