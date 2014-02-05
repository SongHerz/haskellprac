module FS
 ( FSItem (..)
 , FSZipper
 , myDisk
 , fsUp
 , fsTo
 , fsRename
 , fsNewFile
 ) where

import Data.List (break)

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk =
    Folder "Root"
        [ File "goat_yelling_like_man.wmv" "baaaaa"
        , File "pope_time.avi" "god bless"
        , Folder "pics"
            [ File "ape_throwing_up.jpg" "bleargh"
            , File "watermelon_smash.gif" "smash!!"
            , File "skull_man(scary).bmp" "Yikes!"
            ]
        , File "dijon_poupon.doc" "best mustard"
        , Folder "programs"
            [ File "fartwizard.exe" "10gotofart"
            , File "owl_bandit.dmg" "mov eax, h00t"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source code"
                [ File "best_hs_prog.hs" "main = print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]


data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
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
fsAttach item (File _ _) = Nothing

-- Remove an item with given name in the current focus
fsRemove :: Name -> FSZipper -> Maybe FSZipper
fsRemove name (Folder folderName items, bs) =
    case break (nameIs name) items of
        (_, []) -> Nothing
        (ls, item:rs) -> Just (Folder folderName ls ++ rs, bs)


