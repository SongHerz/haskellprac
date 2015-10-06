module Heap (
      Heap
    , Dir (..) -- For test only
    , dirs     -- For test only
    , empty
    , singleton
    , insert
    ) where

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
        deriving (Eq, Read, Show)

singletonNode x = Node { prio = x, left = Empty, right = Empty }

-- fstNodeWithEmptyChild is always the node with 
data Heap a =
        Heap { root :: Node a
             , size :: Int }
        deriving (Eq, Read, Show)

-- Direction for traversing left or right
data Dir = L | R
         deriving (Eq, Show)

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

-- Create an heap with only one node
singleton :: a -> Heap a
singleton x = Heap { root = singletonNode x, size = 1}

insert :: Ord a => a -> Heap a -> Heap a
insert x (Heap Empty 0) = singleton x
insert x h = Heap { root = _insert x (root h) parent_dirs, size = new_size }
    where new_size = size h + 1
          n_parent = div new_size 2
          parent_dirs = dirs n_parent

_insert :: Ord a => a -> Node a -> [Dir] -> Node a
_insert x node [] = if prio node <= x
                    then addChild (prio node) x node
                    else addChild x (prio node) node
_insert x node (d:dirs) = if prio node <= prio new_child
                          then new_node
                          else prio_swapped_new_node
    where child = case d of
                      L -> left node
                      R -> right node
          new_child = _insert x child dirs
          new_node = case d of
                         L -> node { left = new_child }
                         R -> node { right = new_child }

          -- new_node is a node whose priority has been swapped with
          -- its child
          prio_swapped_new_node =
              case d of
                  L -> node { prio = prio new_child, left = new_child { prio = prio node }}
                  R -> node { prio = prio new_child, right = new_child { prio = prio node }}

-- Add a child node to this node, and return the modified node.
-- this_new_prio : new priority for this node
-- child_prio : priority for the child node
addChild :: a -> a -> Node a -> Node a
addChild this_new_prio child_prio node@(Node _ Empty _) = node {prio = this_new_prio, left = singletonNode child_prio}
addChild this_new_prio child_prio node@(Node _ _ Empty) = node {prio = this_new_prio, right = singletonNode child_prio}
