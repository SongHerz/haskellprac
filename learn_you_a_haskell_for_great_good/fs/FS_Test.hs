module FS_Test where

import FS
import Test.HUnit

picsFolder :: FSItem
picsFolder = 
    Folder "pics"
        [ File "ape_throwing_up.jpg" "bleargh"
        , File "watermelon_smash.gif" "smash!!"
        , File "skull_man(scary).bmp" "Yikes!"
        ]

programsFolder :: FSItem
programsFolder =
    Folder "programs"
        [ File "fartwizard.exe" "10gotofart"
        , File "owl_bandit.dmg" "mov eax, h00t"
        , File "not_a_virus.exe" "really not a virus"
        , Folder "source code"
            [ File "best_hs_prog.hs" "main = print (fix error)"
            , File "random.hs" "main = print 4"
            ]
        ]

myDisk :: FSItem
myDisk =
    Folder "Root"
        [ File "goat_yelling_like_man.wmv" "baaaaa"
        , File "pope_time.avi" "god bless"
        , picsFolder
        , File "dijon_poupon.doc" "best mustard"
        , programsFolder
        ]

test_rootGoUp = TestCase $ assertEqual
    "Should not go up at root" Nothing ( fsUp (myDisk, []))

test_toNonExistItem = TestCase $ assertEqual
    "Should fail on non-exist item" Nothing ( fsTo "NON-EXIST-ITEM" (myDisk, []))

test_normGoUp = TestCase $ do
    let pics_folder_zipper = case fsTo "pics" (myDisk, []) of
                                Just z -> z
    -- go to pic folders first
    assertEqual "Should go to pics folder" picsFolder $ fst pics_folder_zipper 
    -- Then, go up to the root
    let root_folder_zipper = case fsUp pics_folder_zipper of
                                Just z -> z
    assertEqual "Should go to the root now" myDisk $ fst root_folder_zipper
    

tests = TestLabel "Basic Tests" $ TestList
     [ test_rootGoUp, test_toNonExistItem, test_normGoUp ]


main = runTestTT tests
