import Data.Char
import System.IO
import System.Random
import Control.Monad(when)

main = do
    gen <- getStdGen
    askForNumber gen


askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
    putStr "Which number in the range from 1 to 10 am I thinking of? "
    hFlush stdout
    numberString <- getLine
    when (not $ null numberString) $ do
        let inputNumber = case (reads numberString) of
                        [(val, "")]           -> Right val
                        [(val, nonEmptyRest)] -> let nonSpaceChars = ltrim . rtrim $ nonEmptyRest
                                                 in if null nonSpaceChars
                                                        then Right val
                                                        else Left $ "Error: Additional characters \"" ++ nonSpaceChars ++ "\""
                        []                    -> if null numberString
                                                    then Left "Error: No input"
                                                    else Left $ "Error: Non-digit input \"" ++ numberString ++ "\""
                        _                     -> Left "Error: Ambiguous input"

        case inputNumber of
            Right val ->
                if randNumber == val
                    then putStrLn "You are correct!"
                    else putStrLn $ "Sorry, it was " ++ show randNumber
            Left msg -> putStrLn msg
        askForNumber newGen


ltrim = dropWhile isSpace
rtrim = reverse . ltrim . reverse
