import System.Environment
import System.Directory
import System.IO
import Data.List

type Action = (String, [String] -> IO ())

dispatch :: [Action]
dispatch =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            , ("bump", bump)
            ]

main = do
    (command:args) <- getArgs
    case ( lookup command dispatch) of
        Just action -> action args
        otherwise   -> dispatchError command dispatch

dispatchError :: String -> [Action] -> IO ()
dispatchError errAction actionList = do
    let actionStrs = unwords $ map ((\action -> "\"" ++ action ++ "\"") . fst) actionList
    putStrLn ( "Expect " ++ actionStrs ++ ", but got " ++ errAction)

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")


view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks


remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName

bump :: [String] -> IO ()
bump [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        bumpedTask = todoTasks !! number
        newTodoItems = bumpedTask : (delete bumpedTask todoTasks)
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName
