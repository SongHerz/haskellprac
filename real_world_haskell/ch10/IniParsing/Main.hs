module Main where

import System.Environment (getArgs)
import Data.ByteString.Lazy.Char8 as L8

import Parse (parse, prettyPrint)

showIni :: FilePath -> IO ()
showIni iniPath = do
    bs <- L8.readFile iniPath
    Prelude.putStrLn $ prettyStr bs
    where prettyStr bs = case parse bs of
                          Left err -> err
                          Right ini -> prettyPrint ini

main = do
    args <- getArgs
    case args of
        [opt] -> showIni opt
        _     -> Prelude.putStrLn "Usage: app <ini_path>"
        
