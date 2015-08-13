import qualified Data.Map as M
import Control.Monad (when)
import System.Environment (getArgs)
import System.IO (hFlush, stdout, getLine)
import System.Exit (exitFailure)
import Text.Printf (printf)

import qualified Util as U

-- | Fields in a POSIX /etc/passwd file.
data PasswdEntry = PasswdEntry {
      userName :: String
    , password :: String
    , uid :: Integer
    , gid :: Integer
    , gecos :: String
    , homeDir :: String
    , shell :: String
    } deriving (Eq, Ord)

instance Show PasswdEntry where
    show pe = printf "%s:%s:%d:%d:%s:%s:%s"
                (userName pe) (password pe) (uid pe) (gid pe)
                (gecos pe) (homeDir pe) (shell pe)

instance Read PasswdEntry where
    readsPrec _ value =
        case U.split ':' value of
            [f1, f2, f3, f4, f5, f6, f7] ->
                [(PasswdEntry f1 f2 (read f3) (read f4) f5 f6 f7, [])]
            x -> error $ "Invalid number of fields in input: " ++ show x

type UIDMap = M.Map Integer PasswdEntry
type UserMap = M.Map String PasswdEntry

-- | Convert input data to maps.
inputToMaps :: String -> (UIDMap, UserMap)
inputToMaps inp = (uidmap, usermap)
    where entries = map read $ lines inp
          uidmap = M.fromList $ map (\pe -> (uid pe, pe)) entries
          usermap = M.fromList $ map (\pe -> (userName pe, pe)) entries

main = do
    args <- getArgs
    when (length args /= 1) $ do
        putStrLn "Syntax: passwdmap filename"
        exitFailure

    content <- readFile (head args)
    let maps = inputToMaps content
    mainMenu maps

mainMenu maps@(uidmap, usermap) = do
    putStr optionText
    hFlush stdout
    sel <- getLine
    case sel of
        "1" -> lookupUserName >> mainMenu maps
        "2" -> lookupUID >> mainMenu maps
        "3" -> displayFile >> mainMenu maps
        "4" -> return ()
        _ -> putStrLn "Invalid selection" >> mainMenu maps
    where
        lookupUserName = lookupMapWithPrefix "Username: " id usermap
        lookupUID = lookupMapWithPrefix "UID: " read uidmap

        lookupMapWithPrefix :: (Ord k, Show a) => String -> (String -> k) -> M.Map k a -> IO ()
        lookupMapWithPrefix prefix keyStr2Key m = do
            putStrLn prefix
            keystr <- getLine
            lookupMap (keyStr2Key keystr) m

        lookupMap :: (Ord k, Show a) => k -> M.Map k a -> IO ()
        lookupMap k m =
            case M.lookup k m of
                Nothing -> putStrLn "Not found."
                Just x -> print x

        displayFile = putStr . unlines . map (show . snd) . M.toList $ uidmap
        optionText = "\npasswdmap options:\n\
                      \\n\
                      \1   Look up a user name\n\
                      \2   Lookup a UID\n\
                      \3   Display entire file\n\
                      \4   Quit\n\
                      \Your selection: "

