{-# LANGUAGE ScopedTypeVariables #-}

{-
 - I do not think exercises 1 and 2 are good at page 232.
 - As foldTree is to solve the problem that not all entries in a directory
 - need to be traversed.
 - If entries traversal is to be altered, the order function needs to see
 - all entries before determining the traversal order.
 - And exec 1 and 2 are not done at all.
 -
 - For exec 3, I think the intention is to write some simple iterators,
 - and complex iterators can be composed by simple ones.
 - And this file is to solve exec 3.
 -
 - As an Iterate a has 3 data constructors, it is meaningful to define only
 - 'and'/'or' operations.
 -
 - Here is the truth table:
 - it0   and  it1   =
 - Done       Done       Done
 - Done       Skip       Done
 - Done       Continue   Done 
 - Skip       Skip       Skip
 - Skip       Continue   Skip
 - Continue   Continue   Continue
 -
 - it0   or   it1   =
 - Done       Done       Done
 - Done       Skip       Skip
 - Done       Continue   Continue
 - Skip       Skip       Skip
 - Skip       Continue   Continue
 - Continue   Continue   Continue
-}

module FoldDirExec where

import Control.Monad (liftM)
import Data.Char (toLower)
import System.FilePath(
                      (</>)
                      , takeFileName
                      , takeExtension)

import System.Directory (doesFileExist)
import FSCommon (getUsefulContents, Info(..), getInfo, isDirectory)

data Iterate seed = Done     { unwrap :: seed }
                  | Skip     { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)

andIterate :: (Iterate a) -> (Iterate a) -> (Iterate a)
-- Only the 1st done is returned.
-- If seed of two iterates are both done  and not the same, there would be problem.
andIterate done@(Done _) _ = done
andIterate _ done@(Done _) = done
andIterate skip@(Skip _) _ = skip
andIterate _ skip@(Skip _) = skip
andIterate cont@(Continue _) (Continue _) = cont

orIterate :: (Iterate a) -> (Iterate a) -> (Iterate a)
orIterate cont@(Continue _) _ = cont
orIterate _ cont@(Continue _) = cont
orIterate skip@(Skip _) _ = skip
orIterate _ skip@(Skip _) = skip
orIterate done@(Done _) (Done _) = done

type Iterator seed = seed -> Info -> Iterate seed

liftIterator :: ((Iterate a) -> (Iterate a) -> (Iterate a)) -> (Iterator a) -> (Iterator a) -> (Iterator a)
liftIterator f it0 it1 = \seed info -> it0 seed info `f` it1 seed info

andIterator = liftIterator andIterate
orIterator = liftIterator orIterate

infixr 3 &&?
infixr 2 ||?
(&&?) = andIterator
(||?) = orIterator

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
        fold seed subpath = getUsefulContents subpath >>= walk seed subpath

        -- Use the same type var 'a' as foldTree
        walk :: a -> FilePath -> [FilePath] -> IO (Iterate a)
        walk seed subpath (name:names) = do
            let path' = subpath </> name
            info <- getInfo path'
            case iter seed info of
                done@(Done _)   -> return done
                Skip seed'      -> walk seed' subpath names
                Continue seed'
                    | isDirectory info -> do
                        next <- fold seed' path'
                        case next of
                            done@(Done _)   -> return done
                            seed''          -> walk (unwrap seed'') subpath names
                    | otherwise -> walk seed' subpath names
        walk seed _ _ = return (Continue seed)


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


-- Iterators for testing
extIteratorGenerator :: String -> Iterator [FilePath]
extIteratorGenerator ext paths info
    | isDirectory info
        = Continue paths
    | takeExtension path == ext
        = Continue (path: paths)
    | otherwise
        = Skip paths
    where path = infoPath info


minSizeIteratorGenerator :: Integer -> Iterator [FilePath]
minSizeIteratorGenerator min paths info
    | isDirectory info
        = Continue paths
    | otherwise
        = case liftM (>= min) (infoSize info) of
              Just True -> Continue (path : paths)
              _         -> Skip paths
    where path = infoPath info


pngPics = extIteratorGenerator ".png"
jpgPics = extIteratorGenerator ".jpg" 


-- collect both png and jpg files
foldPngJpg = foldTree (pngPics ||? jpgPics) [] "/tmp/pics"

-- collect png files and size should be >= 1
foldPngWithSizeConstrain = foldTree (pngPics &&? minSizeIteratorGenerator 1) [] "/tmp/"
