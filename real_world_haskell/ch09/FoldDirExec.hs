{-# LANGUAGE ScopedTypeVariables #-}

{-
 - I do not think exercises 1 and 2 are good at page 232.
 - As foldTree is to solve the problem that not all entries in a directory
 - need to be traversed.
 - If entries traversal is to be altered, the order function needs to see
 - all entries before determining the traversal order.
 - And exec 1 and 2 are not done at all.
-}

module FoldDirExec where

import Data.Char (toLower)
import System.FilePath(
                      (</>)
                      , takeFileName
                      , takeExtension)

import FSCommon (getUsefulContents, Info(..), getInfo, isDirectory)

data Iterate seed = Done     { unwrap :: seed }
                  | Skip     { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

foldTree :: forall a. Iterator a -> a -> FilePath -> IO a

foldTree iter initSeed path = do
    endSeed <- fold initSeed path
    return $ unwrap endSeed
    where
        -- Add func signature for better interpretation
        -- Scoped type variable support is required, otherwise
        -- the type var 'a' in this signature is not 'a' in foldTree
        -- signature, and compiles with error.
        fold :: a -> FilePath -> IO (Iterate a)
        fold seed subpath = getUsefulContents subpath >>= walk seed

        -- Use the same type var 'a' as foldTree
        walk :: a -> [FilePath] -> IO (Iterate a)
        walk seed (name:names) = do
            let path' = path </> name
            info <- getInfo path'
            case iter seed info of
                done@(Done _)   -> return done
                Skip seed'      -> walk seed' names
                Continue seed'
                    | isDirectory info -> do
                        next <- fold seed' path'
                        case next of
                            done@(Done _)   -> return done
                            seed''          -> walk (unwrap seed'') names
                    | otherwise -> walk seed' names
        walk seed _ = return (Continue seed)


atMostThreePictures :: Iterator [FilePath]

atMostThreePictures paths info
    | length paths == 3
        = Done paths
    | isDirectory info && takeFileName path == ".svn"
        = Skip paths
    | extension `elem` [".jpg", ".png"]
        = Continue (path: paths)
    | otherwise
        = Continue paths
  where extension = map toLower $ takeExtension path
        path = infoPath info


countDirectories :: Iterator Integer
countDirectories count info =
    Continue (if isDirectory info
              then count + 1
              else count)


        
