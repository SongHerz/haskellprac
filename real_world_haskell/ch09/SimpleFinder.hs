module SimpleFinder (simpleFind) where

import RecursiveContents (getRecursiveContents)

-- simpleFind is strict and not lazy, as explained by Rörd in detail.
-- http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html
--
-- I've thought a long time about what exactly is destroying laziness here.
-- The thing is, yes, the IO Monad forces IO actions to perform
-- sequentially, but why do we actually have to perform the IO actions
-- containing the tail of the list before feeding the head of the list to
-- the caller?
--
-- My conclusion (so far) is that performing the tail happens in forM, more
-- precisely in the function sequence which is called by mapM and therefore
-- by forM. sequence is a foldr, and the function given to the foldr
-- extracts the values from both the current value (representing the head)
-- and the accumulator (representing the tail of the list). Extracting the
-- value from an action performs the action.
--
-- Here's the definition for sequence from Control.Monad:
--
-- sequence ms = foldr k (return []) ms
-- ____________where
-- ______________k m m' = do { x <- m; xs <- m'; return (x:xs) }
--
-- I've tried to come up with a lazy definition for getRecursiveContents,
-- but I was only able to get a version that performs decently on large
-- data by putting an action to perform on each entry directly into the
-- forM function, instead of creating a list of entries.

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]

simpleFind p path = do
    names <- getRecursiveContents path
    return $ filter p names

-- This is very easy to testing by code:
-- $ cat main.hs:
--
-- import SimpleFinder
--
-- main = do
--        files <- simpleFind (\_ -> True) "PATH"
--        print (take 1 files)
--
-- Use /dev as PATH, it shows result quickly.
-- But use /usr instead, it consumes a lot of memory, and delay for a long
-- time.
