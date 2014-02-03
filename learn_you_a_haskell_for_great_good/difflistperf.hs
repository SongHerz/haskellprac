import Control.Monad.Writer
import DiffList

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
    tell $ toDiffList ["0"]
finalCountDown x = do
    finalCountDown $ x - 1
    tell $ toDiffList [show x]


finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = do
    tell ["0"]
finalCountDown' x = do
    finalCountDown' $ x - 1
    tell [show x]
