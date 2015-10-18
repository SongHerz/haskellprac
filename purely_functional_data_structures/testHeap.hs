import Test.QuickCheck

import qualified Heap as H
import Heap (Dir(..))

prop_basic_dirs :: Bool
prop_basic_dirs = all (\p -> H.dirs (fst p) == snd p) n_dirs_pairs
    where n_dirs_pairs = [
                       (1, [])
                     , (2, [L])
                     , (3, [R])
                     , (4, [L, L])
                     , (5, [L, R])
                     , (6, [R, L])
                     , (7, [R, R])
                     , (8, [L, L, L])
                     , (9, [L, L, R])
                     , (10, [L, R, L])
                     , (11, [L, R, R])
                     , (12, [R, L, L])
                     , (13, [R, L, R])
                     , (14, [R, R, L])
                     , (15, [R, R, R])
                     , (16, [L, L, L, L])
                   ]

prop_basic_insert :: Bool
prop_basic_insert = heaps == expected_heaps
    where prios = [1, 3, 5, 7, 9, 6, 8, 10, 2]
          heaps = scanl (flip H.insert) H.empty prios
          expected_heaps = map read [
                                      "Heap {root = Empty, size = 0}"
                                    , "Heap {root = Node {prio = 1, left = Empty, right = Empty}, size = 1}"
                                    , "Heap {root = Node {prio = 1, left = Node {prio = 3, left = Empty, right = Empty}, right = Empty}, size = 2}"
                                    , "Heap {root = Node {prio = 1, left = Node {prio = 3, left = Empty, right = Empty}, right = Node {prio = 5, left = Empty, right = Empty}}, size = 3}"
                                    , "Heap {root = Node {prio = 1, left = Node {prio = 3, left = Node {prio = 7, left = Empty, right = Empty}, right = Empty}, right = Node {prio = 5, left = Empty, right = Empty}}, size = 4}"
                                    , "Heap {root = Node {prio = 1, left = Node {prio = 3, left = Node {prio = 7, left = Empty, right = Empty}, right = Node {prio = 9, left = Empty, right = Empty}}, right = Node {prio = 5, left = Empty, right = Empty}}, size = 5}"
                                    , "Heap {root = Node {prio = 1, left = Node {prio = 3, left = Node {prio = 7, left = Empty, right = Empty}, right = Node {prio = 9, left = Empty, right = Empty}}, right = Node {prio = 5, left = Node {prio = 6, left = Empty, right = Empty}, right = Empty}}, size = 6}"
                                    , "Heap {root = Node {prio = 1, left = Node {prio = 3, left = Node {prio = 7, left = Empty, right = Empty}, right = Node {prio = 9, left = Empty, right = Empty}}, right = Node {prio = 5, left = Node {prio = 6, left = Empty, right = Empty}, right = Node {prio = 8, left = Empty, right = Empty}}}, size = 7}"
                                    , "Heap {root = Node {prio = 1, left = Node {prio = 3, left = Node {prio = 7, left = Node {prio = 10, left = Empty, right = Empty}, right = Empty}, right = Node {prio = 9, left = Empty, right = Empty}}, right = Node {prio = 5, left = Node {prio = 6, left = Empty, right = Empty}, right = Node {prio = 8, left = Empty, right = Empty}}}, size = 8}"
                                    , "Heap {root = Node {prio = 1, left = Node {prio = 2, left = Node {prio = 3, left = Node {prio = 10, left = Empty, right = Empty}, right = Node {prio = 7, left = Empty, right = Empty}}, right = Node {prio = 9, left = Empty, right = Empty}}, right = Node {prio = 5, left = Node {prio = 6, left = Empty, right = Empty}, right = Node {prio = 8, left = Empty, right = Empty}}}, size = 9}" ]

prop_basic_delete_min :: Bool
prop_basic_delete_min = heaps == expected_heaps
    where prios = [1, 3, 5, 7, 9, 6, 8, 10, 2]
          heap = foldl (flip H.insert) H.empty prios
          heaps = take (length prios + 1) $ iterate H.deleteMin heap
          expected_heaps = map read [
                                      "Heap {root = Node {prio = 1, left = Node {prio = 2, left = Node {prio = 3, left = Node {prio = 10, left = Empty, right = Empty}, right = Node {prio = 7, left = Empty, right = Empty}}, right = Node {prio = 9, left = Empty, right = Empty}}, right = Node {prio = 5, left = Node {prio = 6, left = Empty, right = Empty}, right = Node {prio = 8, left = Empty, right = Empty}}}, size = 9}"
                                    , "Heap {root = Node {prio = 2, left = Node {prio = 3, left = Node {prio = 7, left = Node {prio = 10, left = Empty, right = Empty}, right = Empty}, right = Node {prio = 9, left = Empty, right = Empty}}, right = Node {prio = 5, left = Node {prio = 6, left = Empty, right = Empty}, right = Node {prio = 8, left = Empty, right = Empty}}}, size = 8}"
                                    , "Heap {root = Node {prio = 3, left = Node {prio = 7, left = Node {prio = 10, left = Empty, right = Empty}, right = Node {prio = 9, left = Empty, right = Empty}}, right = Node {prio = 5, left = Node {prio = 6, left = Empty, right = Empty}, right = Node {prio = 8, left = Empty, right = Empty}}}, size = 7}"
                                    , "Heap {root = Node {prio = 5, left = Node {prio = 7, left = Node {prio = 10, left = Empty, right = Empty}, right = Node {prio = 9, left = Empty, right = Empty}}, right = Node {prio = 6, left = Node {prio = 8, left = Empty, right = Empty}, right = Empty}}, size = 6}"
                                    , "Heap {root = Node {prio = 6, left = Node {prio = 7, left = Node {prio = 10, left = Empty, right = Empty}, right = Node {prio = 9, left = Empty, right = Empty}}, right = Node {prio = 8, left = Empty, right = Empty}}, size = 5}"
                                    , "Heap {root = Node {prio = 7, left = Node {prio = 9, left = Node {prio = 10, left = Empty, right = Empty}, right = Empty}, right = Node {prio = 8, left = Empty, right = Empty}}, size = 4}"
                                    , "Heap {root = Node {prio = 8, left = Node {prio = 9, left = Empty, right = Empty}, right = Node {prio = 10, left = Empty, right = Empty}}, size = 3}"
                                    , "Heap {root = Node {prio = 9, left = Node {prio = 10, left = Empty, right = Empty}, right = Empty}, size = 2}"
                                    , "Heap {root = Node {prio = 10, left = Empty, right = Empty}, size = 1}"
                                    , "Heap {root = Empty, size = 0}" ]

prop_basic_nodesbfs :: Bool
prop_basic_nodesbfs = nodes == expected_nodes
    where prios = [1, 3, 5, 7, 9, 6, 8, 10, 2]
          heap = foldl (flip H.insert) H.empty prios
          nodes = H.nodesbfs heap
          expected_nodes = read "[Node {prio = 1, left = Node {prio = 2, left = Node {prio = 3, left = Node {prio = 10, left = Empty, right = Empty}, right = Node {prio = 7, left = Empty, right = Empty}}, right = Node {prio = 9, left = Empty, right = Empty}}, right = Node {prio = 5, left = Node {prio = 6, left = Empty, right = Empty}, right = Node {prio = 8, left = Empty, right = Empty}}},Node {prio = 2, left = Node {prio = 3, left = Node {prio = 10, left = Empty, right = Empty}, right = Node {prio = 7, left = Empty, right = Empty}}, right = Node {prio = 9, left = Empty, right = Empty}},Node {prio = 5, left = Node {prio = 6, left = Empty, right = Empty}, right = Node {prio = 8, left = Empty, right = Empty}},Node {prio = 3, left = Node {prio = 10, left = Empty, right = Empty}, right = Node {prio = 7, left = Empty, right = Empty}},Node {prio = 9, left = Empty, right = Empty},Node {prio = 6, left = Empty, right = Empty},Node {prio = 8, left = Empty, right = Empty},Node {prio = 10, left = Empty, right = Empty},Node {prio = 7, left = Empty, right = Empty}]"

prop_complete_node :: Bool
prop_complete_node = all (\(n, r) -> H.isCompleteBinary n == r) node_iscomplete_pairs
    where node_iscomplete_pairs = [
                (H.Empty, True)
              , (H.Node 1 H.Empty H.Empty, True)
              , (H.Node 1 (H.Node 2 H.Empty H.Empty) H.Empty, True)
              , (H.Node 1 H.Empty (H.Node 2 H.Empty H.Empty), False)
              , (H.Node 1 (H.Node 2 H.Empty H.Empty) (H.Node 3 H.Empty H.Empty), True)
              , (H.Node 1 (H.Node 2 H.Empty H.Empty)
                          (H.Node 3 (H.Node 4 H.Empty H.Empty)
                                    H.Empty)
                 , False)
              ]

runTests :: Args -> IO ()
runTests args = do
    mapM_ (quickCheckWithResult args) [ prop_basic_dirs
                                      , prop_basic_insert
                                      , prop_basic_delete_min
                                      , prop_basic_nodesbfs
                                      , prop_complete_node
                                      ]
    return ()

args = Args {
      replay = Nothing
    , maxSuccess = 200
    , maxDiscardRatio = 100
    , maxSize = 100
    , chatty = True
    }

main = runTests args
