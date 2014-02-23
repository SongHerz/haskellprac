data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

leftNode (Node _ l _)   = Just l
leftNode Empty          = Nothing

rightNode (Node _ _ r)  = Just r
rightNode Empty         = Nothing

fromJust (Just n) = n

nodesAreSame (Node a _ _) (Node b _ _)
    | a == b     = Just a
nodesAreSame _ _ = Nothing


simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)

height :: Tree a -> Int
height Empty        = 0
height (Node _ a b) = let heightA = height a
                          heightB = height b
                      in 1 + if heightA > heightB
                             then heightA
                             else heightB


data MaybeTree a = MaybeTree a (Maybe (MaybeTree a)) (Maybe (MaybeTree a))
                   deriving (Show)

simpleMaybeTree = MaybeTree "parent" (Just (MaybeTree "left child" Nothing Nothing))
                                     (Just (MaybeTree "right child" Nothing Nothing))
