module FSCommon (
    getUsefulContents,
    Info(..),
    getInfo
) where


import Control.Monad (liftM)
import Control.Exception (handle, bracket)
import System.Directory (Permissions(..),
        getDirectoryContents, getPermissions, getModificationTime)
import System.IO (IOMode(ReadMode), openFile, hClose, hFileSize)
import Data.Time.Clock (UTCTime)

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return $ filter (`notElem` [".", ".."]) names 


data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe UTCTime
    } deriving (Eq, Ord, Show)


maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle ((\_ -> return Nothing) :: IOError -> IO (Maybe a)) (Just `liftM` act)


getInfo :: FilePath -> IO Info
getInfo path = do
    perms <- maybeIO $ getPermissions path
    size <- maybeIO $ bracket (openFile path ReadMode) hClose hFileSize
    modified <- maybeIO $ getModificationTime path
    return $ Info path perms size modified
