{-
For a complete binary tree of h-level:
1. There are at most (2^h - 1) nodes.
2. For the k-th level, there are (2^(k - 1)) nodes.
3. For a i-th (i counted from 1) node,
   its children number are (i*2) and (i*2 + 1),
   its parent number is (i/2).

For a complete binary tree of n-nodes:
Denote the height of the tree as h, the last node resides at h-level.
h - 1 = floor(log2(n))

Denote number of nodes in 1 ~ (h-1) levels as n0:
n0 = 2^(h - 1) - 1

Number of nodes at the last level:
n1 = n - n0

Number of nodes at h-th level, when h-th level is full:
nh = 2^(h - 1)

The last node is at the left side of the tree, when
n1 <= nh / 2
The last node is at the right side of the tree, when
n1 > nh / 2
-}

data Node a = Empty
          | Node { prio :: a
                 , left :: Node a
                 , right :: Node a }

-- fstNodeWithEmptyChild is always the node with 
data Heap a =
        Heap { root :: Node a
             , size :: Int }

-- Direction for traversing left or right
data Dir = L | R
         deriving (Show)

-- Give an node number, get a list of directions for node traversal.
dirs :: Int -> [Dir]
dirs 1 = []
dirs 2 = [L]
dirs 3 = [R]
dirs n = let 
             -- h is the level that node #n resides.
             h_minus_1 = floor $ logBase 2.0 $ fromIntegral n
             -- number of nodes at the h-th level, when h-th level is full.
             nh = 2 ^ h_minus_1
             -- number of nodes in 1 ~ (h - 1) levels.
             n0 = nh - 1
             -- number of nodes before node #n (including n)
             n1 = n - n0
             -- The direction, and nodes should be removed
             (d, n_elim) = if n1 <= div nh 2 
                               then (L, (2 ^ (h_minus_1 - 1) - 1) + 1)
                               else (R, (2 ^ h_minus_1 - 1) + 1)
         in d : dirs (n - n_elim)

-- Create an empty heap
empty :: Heap a
empty = Heap { root = Empty, size = 0 }

-- inert :: a -> Heap a -> Heap a
-- insert x h =
