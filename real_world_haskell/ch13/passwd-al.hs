import Control.Monad (when)
import System.Exit (exitFailure)
import System.Environment (getArgs)

import Util (split)

main = do
    args <- getArgs

    when (length args /= 2) $ do
        putStrLn "Syntax: passwd-al filename uid"
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

