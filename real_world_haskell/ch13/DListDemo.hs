-- To view results of each DList data,
-- use DL.toList
import qualified DList as DL

xs = DL.fromList [1, 2, 3]
xs2 = xs `DL.append` (DL.fromList [4, 5, 6])
xs3 = 0 `DL.cons` xs2
s = DL.dfoldr (+) 0 xs3
h = DL.safeHead xs3
h2 = DL.safeHead DL.empty
xs4 = fmap (*2) xs3
