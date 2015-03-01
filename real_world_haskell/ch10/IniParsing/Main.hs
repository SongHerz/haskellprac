module Main where

import System.Environment (getArgs)
import Data.ByteString.Lazy.Char8 as L8

import Parse (parse)

showIni :: FilePath -> IO ()
showIni iniPath = do
    bs <- L8.readFile iniPath
    Prelude.putStrLn $ show $ parse bs

main = do
    args <- getArgs
    case args of
        [opt] -> showIni opt
        _     -> Prelude.putStrLn "Usage: app <ini_path>"
        
