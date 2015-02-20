module FSCommon (getUsefulContents
                , Info(..)
                , getInfo
                , isDirectory
) where


import Control.Monad (liftM)
import Control.Exception (handle, bracket)

{-
- exercise 2 at page 234 asks to get entry owner.
- To make code portable entry owner is got from PosixCompat library,
- which is a portable works on both Linux and Windows
-}
import System.PosixCompat.Files (getFileStatus, fileOwner)
import System.PosixCompat.User (UserEntry(..), getUserEntryForID)

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
    , infoOwner :: Maybe String
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe UTCTime
    } deriving (Eq, Ord, Show)


maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle ((\_ -> return Nothing) :: IOError -> IO (Maybe a)) (Just `liftM` act)


getInfo :: FilePath -> IO Info
getInfo path = do
    -- FIXME: other fields can also get from PoxisComat library
    --        For simplicity not do it now
    owner <- maybeIO $ getFileStatus path >>= (getUserEntryForID . fileOwner) >>= (return . userName)
    perms <- maybeIO $ getPermissions path
    size <- maybeIO $ bracket (openFile path ReadMode) hClose hFileSize
    modified <- maybeIO $ getModificationTime path
    return $ Info path owner perms size modified


isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms
