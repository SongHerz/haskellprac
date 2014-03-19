import Data.List (isPrefixOf)

dlts :: String -> [String]
dlts = foldr step [] . lines
    where step l ds
            | "#define DLT_" `isPrefixOf` l = secondWord l : ds
            | otherwise                     = ds
          secondWord = head . tail . words

macros = unlines [ "#define DLT_EN10MB   1 /*..*/",
                   "#define DLT_EN3MB    2 /*...*/",
                   "#define DLT_AX25     3 /*...*/"]

main = putStrLn $ show $ dlts macros
