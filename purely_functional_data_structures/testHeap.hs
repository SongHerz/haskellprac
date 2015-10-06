import Test.QuickCheck

import Heap (dirs, Dir(..))

prop_basic_dirs :: Bool
prop_basic_dirs = all (\p -> dirs (fst p) == snd p) n_dirs_pairs
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


runTests :: Args -> IO ()
runTests args = do
    quickCheckWithResult args prop_basic_dirs
    return ()

args = Args {
      replay = Nothing
    , maxSuccess = 200
    , maxDiscardRatio = 100
    , maxSize = 100
    , chatty = True
    }

main = runTests args
