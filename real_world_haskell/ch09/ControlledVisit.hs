import Data.List (sortBy)
import Control.Monad (liftM, forM, mapM)
import Control.Exception (handle, bracket)
import System.FilePath ((</>))
import System.Directory (Permissions(..),
         getDirectoryContents, getPermissions, getModificationTime)
import System.IO (IOMode(ReadMode), openFile, hClose, hFileSize)
import Data.Time.Clock (UTCTime)

data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe UTCTime
    } deriving (Eq, Ord, Show)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> do
        if isDirectory info && infoPath info /= path
            then traverse order (infoPath info)
            else return [info]


getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return $ filter (`notElem` [".", ".."]) names 


isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms


maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle ((\_ -> return Nothing) :: IOError -> IO (Maybe a)) (Just `liftM` act)

getInfo path = do
    perms <- maybeIO $ getPermissions path
    size <- maybeIO $ bracket (openFile path ReadMode) hClose hFileSize
    modified <- maybeIO $ getModificationTime path
    return $ Info path perms size modified


{-
 - Exercise 1. Page 228.
 - Traverse a path in reverse alphabetic order.
 -}
reverseAlphabeticTraverse path = traverse (sortBy rPathCmp) path
    where rPathCmp a b = reverseOrd $ compare (infoPath a) (infoPath b)
          reverseOrd EQ = EQ
          reverseOrd GT = LT
          reverseOrd LT = GT
