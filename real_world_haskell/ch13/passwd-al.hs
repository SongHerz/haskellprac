import Control.Monad (when)
import System.Exit (exitFailure)
import System.Environment (getArgs)

main = do
    args <- getArgs

    when (length args /= 2) $ do
        putStrLn "Syntax: passwd-al filename uild"
        exitFailure

    content <- readFile (args !! 0)
    let uid = read $ args !! 1
    let username = findByUID content uid

    case username of
        Just x -> putStrLn x
        Nothing -> putStrLn $ "Could not find UID " ++ show uid

findByUID :: String -> Integer -> Maybe String
findByUID content uid =
        let al = map parseLine $ lines content
            in lookup uid al

parseLine :: String -> (Integer, String)
parseLine line = let fields = split ':' line
                     in (read (fields !! 2), fields !! 0) 

split :: Eq a => a -> [a] -> [[a]]
split delim str = 
        let (before, reminder) = span (/= delim) str
            in
            if null reminder
               then case before of
                        [] -> []
                        x  -> [x]
               else before : case tail reminder of
                                   [] -> [[]]
                                   y -> split delim y
