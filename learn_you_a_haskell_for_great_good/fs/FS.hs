module FS
 ( FSItem (..)
 , FSZipper
 , fsUp
 , fsTo
 , fsRename
 , fsAttach
 , fsRemove
 ) where

import Data.List (break)

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show, Eq)


data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show, Eq)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> Maybe FSZipper
fsUp (item, FSCrumb name ls rs:bs) = Just (Folder name (ls ++ [item] ++ rs), bs)
fsUp (_, []) = Nothing

fsTo :: Name -> FSZipper -> Maybe FSZipper
fsTo name (Folder folderName items, bs) = 
    case break (nameIs name) items of
        (_, []) -> Nothing
        (ls, item:rs) -> Just (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

-- Rename current focused item with given name
fsRename :: Name -> FSZipper -> Maybe FSZipper
fsRename newName (Folder name items, bs) = Just (Folder newName items, bs)
fsRename newName (File name dat, bs) = Just (File newName dat, bs)

-- attach a new item in the current focus
fsAttach :: FSItem -> FSZipper -> Maybe FSZipper
fsAttach item (Folder folderName items, bs) =
    Just (Folder folderName (item:items), bs)
fsAttach item (File _ _, _) = Nothing

-- Remove an item with given name in the current focus
fsRemove :: Name -> FSZipper -> Maybe FSZipper
fsRemove name (Folder folderName items, bs) =
    case break (nameIs name) items of
        (_, []) -> Nothing
        (ls, item:rs) -> Just (Folder folderName (ls ++ rs), bs)


