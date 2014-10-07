import Control.Monad (forM, filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import Data.Time.Clock (UTCTime(..))
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

import RecursiveContents (getRecursiveContents)

type InfoP a = FilePath         -- path to directory entry
            -> Permissions      -- permissions
            -> Maybe Integer    -- file size (Nothing if not file)
            -> UTCTime          -- last modified
            -> a

type Predicate = InfoP Bool

getFileSize :: FilePath -> IO (Maybe Integer)

getFileSize path = handle ((\_ -> return Nothing) :: IOError -> IO (Maybe Integer)) $
    bracket (openFile path ReadMode) hClose $ \h -> do
        size <- hFileSize h
        return (Just size)


betterFind :: Predicate -> FilePath -> IO [FilePath]

betterFind p path = getRecursiveContents path >>= filterM check
    where check name = do
            perms    <- getPermissions name
            size     <- getFileSize name
            modified <- getModificationTime name
            return $ p name perms size modified




pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

{-
 - Actually, "InfoP a -> a -> InfoP Bool" equals
 - "InfoP a -> a -> (FilePath -> Permissions -> Maybe Integer -> UTCTime -> Bool)"
 -
 - Because a function type signature is right-associative,
 - the above signature equals:
 - "InfoP a -> a -> FilePath -> Permissions -> Maybe Integer -> UTCTime -> Bool"
 -
 - And we can write equalP' below.
 -
 - Here is parameters and typem mapping.
 - "InfoP a -> a -> FilePath -> Permissions -> Maybe Integer -> UTCTime -> Bool"
 -    f        k       w             x              y             z
 -
 - However, actually I prefer the equalP version, because I think it is more readable.
 -
 - Reference: http://stackoverflow.com/questions/4768453/type-signature-vs-function-equation-in-haskell
 - See the last comment by 'Or When'
 -
 -}
equalP' :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP' f k w x y z = f w x y z == k

-- liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
-- liftP q f k w x y z = f w x y z `q` k
--
-- I prefer this form
liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k = \w x y z -> f w x y z `q` k


greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP  = liftP (<)

simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g = \w x y z -> f w x y z && g w x y z

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g = \w x y z -> f w x y z `q` g w x y z

andP = liftP2 (&&)
orP  = liftP2 (||)

constP :: a -> InfoP a
constP k = \_ _ _ _ -> k

-- Rewrite liftP by liftP2
liftP' :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP' q f k = liftP2 q f (constP k)


myTest :: InfoP Bool
myTest path _ (Just size) _ = takeExtension path == ".cpp" && size > 1024 * 128
myTest _ _ _ _ = False


liftPath :: (FilePath -> a) -> InfoP a
liftPath q = \path _ _ _ -> q path

myTest2 :: InfoP Bool
myTest2 = (liftPath takeExtension `equalP` ".cpp") `andP`
          (sizeP `greaterP` (1024 * 128))


