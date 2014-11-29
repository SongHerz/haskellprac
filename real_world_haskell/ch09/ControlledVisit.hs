import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Monad (liftM, forM, mapM)
import Control.Exception (handle, bracket)
import System.FilePath ((</>))
import System.Directory (Permissions(..),
         getDirectoryContents, getPermissions, getModificationTime)
import System.IO (IOMode(ReadMode), openFile, hClose, hFileSize)
import Data.Time.Clock (UTCTime)

import FSCommon (getUsefulContents, Info(..), getInfo)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> do
        if isDirectory info && infoPath info /= path
            then traverse order (infoPath info)
            else return [info]


isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms


{-
 - Exercise 1. Page 228.
 - Traverse a path in reverse alphabetic order.
 - Use: traverse reverseAlphabeticPathOrder path
 -}
-- reverseAlphabeticPathOrder ::  [Info] -> [Info]
-- reverseAlphabeticPathOrder = sortBy rPathCmp
--     where rPathCmp a b = reverseOrd $ compare (infoPath a) (infoPath b)
--           reverseOrd EQ = EQ
--           reverseOrd GT = LT
--           reverseOrd LT = GT

{-
 - There is another implementation of Exercise 1.
 - This is more elegant from:
 - http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html
 - Frederic Dumont 2009-04-13
 -}
reverseAlphabeticPathOrder ::  [Info] -> [Info]
reverseAlphabeticPathOrder = sortBy (flip $ comparing infoPath)

{-
 - Exercise 2. Page 228.
 - Put the parent last.
 - Use: traverse childrenFirstOrder path
 -}
childrenFirstOrder ::  [Info] -> [Info]
childrenFirstOrder infos = tail infos ++ (head infos : [])


{-
 - Exercise 3. Page 228.
 -}
type InfoP a = Info -> a
type Predicate = InfoP Bool

pathP :: Info -> FilePath
pathP info = infoPath info

sizeP :: Info -> Integer
sizeP info = case infoSize info of
                Just size -> size
                Nothing   -> -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \info -> f info == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k = \info -> f info `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP  = liftP (<)


simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g = \info -> f info && g info

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g = \info -> f info `q` g info

andP = liftP2 (&&)
orP = liftP2 (||)

constP :: a -> InfoP a
constP k = \_ -> k

-- Rewrite liftP by liftP2
liftP' :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP' q f k = liftP2 q f (constP k)

infix 4 ==?
infixr 3 &&?
infixr 2 ||?
infix 4 >?
infix 4 <?

(==?) :: (Eq a) => InfoP a -> a -> InfoP Bool
(==?) = equalP

(&&?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(&&?) = andP

(||?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(||?) = orP

(>?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(>?)  = greaterP

(<?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(<?)  = lesserP


{-
 - Exercise 4. Page 228.
 -}
traverse' :: Predicate -> Predicate -> FilePath -> IO [Info]
traverse' predt predo path = do
    infos <- traverse (filter predt) path
    return $ filter predo infos
