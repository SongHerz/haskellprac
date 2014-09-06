str2action :: String -> IO ()
str2action = putStrLn . (++) "Data: "

numbers :: [Int]
numbers = [1..10]

main = do str2action "Start of the program"
          mapM_ (str2action . show) numbers
          str2action "Done!"
