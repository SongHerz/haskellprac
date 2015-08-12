import qualified Data.Map as M
import Control.Monad (when)
import System.Environment (getArgs)
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

mainMenu maps@(uidmap, usermap) = error "" -- FIXME: FINISH THIS

