module GlobPortable (namesMatching) where

import Data.List (isInfixOf)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)
import System.FilePath (pathSeparator,
                        dropTrailingPathSeparator, splitFileName, (</>))

import Control.Exception (handle)
import Control.Monad (forM, filterM)
import qualified GlobRegexCase as GRC (matchesGlob)

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
allSubDirs :: FilePath -> IO [FilePath]
allSubDirs root = do
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
namesMatching pat
    | not (isPattern pat) = do
        exists <- doesNameExist pat
        return (if exists then [pat] else [])
    | otherwise = do
        case splitFileName pat of
            -- NOTE: At least for ghc 7.8.3, splitFileName returns 
            -- (non_empty_current_dir_path, baseName) when there is
            -- only basename in the path.
            -- And here will never be covered.
            ("", baseName) -> do
                curDir <- getCurrentDirectory
                listMatches curDir baseName
            (dirName, baseName) -> do
                dirs <- if isPattern dirName
                        then namesMatching (dropTrailingPathSeparator dirName)
                        else return [dirName]
                let listDir = if isPattern baseName
                              then listMatches
                              else listPlain
                pathNames <- forM dirs $ \dir -> do
                                baseNames <- listDir dir baseName
                                return (map (dir </>) baseNames)
                return (concat pathNames)


doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists
        then return True
    else doesDirectoryExist name


listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    handle (const (return []) :: IOError -> IO [String]) $ do
        names <- getDirectoryContents dirName'
        let names' = if isHidden pat
                     then filter isHidden names
                     else filter (not . isHidden) names
        let matchesGlob = if pathSeparator == '/'
                          then (\name pat -> GRC.matchesGlob name pat False)
                          else (\name pat -> GRC.matchesGlob name pat True)
        return $ filter (`matchesGlob` pat) names'


isHidden ('.':_) = True
isHidden _       = False


listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
    exists <- if null baseName
              then doesDirectoryExist dirName
              else doesNameExist (dirName </> baseName)
    return $ if exists then [baseName] else []
