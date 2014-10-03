{-
 - FIXME: For "**.ext" feature, a directory will be scanned twice.
 -        Maybe I should change the code to make it scan only once.
 -}
module GlobPortableEither (namesMatching) where

import Data.List (isInfixOf)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)
import System.FilePath (pathSeparator,
                        dropTrailingPathSeparator, splitFileName, (</>))

import Control.Exception (handle)
import Control.Monad (forM, filterM)
import qualified GlobRegexCaseEither as GRC (matchesGlob, GlobError)

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

-- Check if a string contains "**" pattern
isDeepSearchPattern :: String -> Bool
isDeepSearchPattern = isInfixOf "**"

-- Convert "**" in a pattern to one "*"
-- FIXME: I have to check if this is necessary,
--        but it seems we'd better do this.
normalizePattern :: String -> String
normalizePattern ('*':'*':xs) = normalizePattern ('*':xs)
normalizePattern (x : xs)     = x : normalizePattern xs
normalizePattern []           = []

-- Return a list that contains the directory
-- and all its sub-directories recursively.
allDirs :: FilePath -> IO [FilePath]
allDirs root = do
    subDirs <- allSubDirs' root
    return (root : subDirs)


-- This will not add root to the final dir list
allSubDirs' :: FilePath -> IO [FilePath]
allSubDirs' root = do
    entries <- getDirectoryContents root
    let entryPaths = map (root </>) entries
    -- We should also filter out "." and ".."
    dirs <- filterM (\dirPath -> do
                            let isDotDir =(snd.splitFileName $ dirPath) `elem` [".", ".."]
                            isDir <- doesDirectoryExist dirPath
                            return $ not isDotDir && isDir)
                    entryPaths
    subDirsList <- forM dirs allSubDirs'
    return $ concat ( dirs : subDirsList)

-- Only support glob pattern, and '**.c'
namesMatching :: String -> IO (Either GRC.GlobError [FilePath])
namesMatching pat
    | not (isPattern pat) = do
        exists <- doesNameExist pat
        return $ Right $ (if exists then [pat] else [])
    | otherwise = do
        case splitFileName pat of
            -- NOTE: At least for ghc 7.8.3, splitFileName returns 
            -- (non_empty_current_dir_path, baseName) when there is
            -- only basename in the path.
            -- And here will never be covered.
            ("", baseName) -> do
                curDir <- getCurrentDirectory
                dirFileNamePairs <- listMatches curDir baseName
                case dirFileNamePairs of
                    Right xs  -> return $ Right $ map (\(dirPath, fileName) -> dirPath </> fileName) xs
                    Left  err -> return $ Left err
            (dirName, baseName) -> do
                dirList <- if isPattern dirName
                        then namesMatching (dropTrailingPathSeparator dirName)
                        else return $ Right [dirName]
                case dirList of
                    Left err -> return $ Left err
                    Right dirs -> do
                        let listDir = if isPattern baseName
                                      then listMatches
                                      else listPlain
                        pathNames <- foldr (\dir ioEither -> do
                                        acc <- ioEither
                                        case acc of
                                            Right pathLists -> do
                                                dirFileNamePairs <- listDir dir baseName
                                                case dirFileNamePairs of
                                                    Right xs ->  return $ Right $ (map (\(dirPath, fileName) -> dirPath </> fileName) xs) : pathLists
                                                    Left  err -> return $ Left err
                                            Left err -> return $ Left err) (return $ Right []) dirs
                        return $ case pathNames of
                                   Right xs  -> Right $ concat xs
                                   Left  err -> Left err


doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists
        then return True
    else doesDirectoryExist name


listMatches :: FilePath -> String -> IO (Either GRC.GlobError [(FilePath, FilePath)])
listMatches dirName pat
    | isDeepSearchPattern pat = do
        let pat' = normalizePattern pat
        dirs <- allDirs dirName
        fileLists <- foldr (\dir ioEither -> do
                                either <- ioEither
                                case either of
                                    Right fileLists -> do
                                        eitherFileList <- listMatchesForOneDir dir pat'
                                        case eitherFileList of
                                            Right fileList -> return $ Right $ fileList : fileLists
                                            Left  err      -> return $ Left err
                                    Left err -> return $ Left err) (return $ Right []) dirs
        case fileLists of
            Right xs  -> return $ Right $ concat xs
            Left  err -> return $ Left err
    | otherwise = listMatchesForOneDir dirName pat    


listMatchesForOneDir :: FilePath -> String -> IO (Either GRC.GlobError [(FilePath, FilePath)])
listMatchesForOneDir dirName pat = do
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    handle (const (return $ Right []) :: IOError -> IO (Either GRC.GlobError [(FilePath, FilePath)])) $ do
        names <- getDirectoryContents dirName'
        let names' = if isHidden pat
                     then filter isHidden names
                     else filter (not . isHidden) names
        let filteredNames = foldr isNameMatch (Right []) names'
        case filteredNames of
            Right xs  -> return $ Right $ map (\fileName -> (dirName', fileName)) xs
            Left  err -> return $ Left err
        where isNameMatch :: String -> Either GRC.GlobError [String] -> Either GRC.GlobError [String]
              isNameMatch _ (Left err) = Left err
              isNameMatch name (Right xs) =
                case name `matchesGlob` pat of
                    Right True   -> Right $ name : xs
                    Right False  -> Right xs
                    Left  err    -> Left err
                where matchesGlob = if pathSeparator == '/'
                                    then (\name pat -> GRC.matchesGlob name pat False)
                                    else (\name pat -> GRC.matchesGlob name pat True)


isHidden ('.':_) = True
isHidden _       = False


-- This always success
listPlain :: FilePath -> String -> IO (Either GRC.GlobError [(FilePath, FilePath)])
listPlain dirName baseName = do
    exists <- if null baseName
              then doesDirectoryExist dirName
              else doesNameExist (dirName </> baseName)
    return $ Right $ if exists then [(dirName, baseName)] else []
