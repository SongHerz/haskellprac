import Supply
import System.Random hiding (next)
import Control.Monad

randomsIO :: Random a => IO [a]
randomsIO =
    getStdRandom $ \g ->
        let (a, b) = split g
        in (randoms a, b)

oneRandom :: IO (Maybe Int)
oneRandom = do
    xs <- randomsIO
    return $ fst $ runSupply next xs

-- Another way to implement oneRandom
oneRandom' :: IO (Maybe Int)
oneRandom' = (fst . runSupply next) `liftM` randomsIO

oneRandom'' :: IO (Maybe Int)
oneRandom'' = (fst . runSupply next) `fmap` randomsIO

twoRandoms :: IO [Maybe Int]
twoRandoms = do
    xs <- randomsIO
    return $ fst $ runSupply (liftM2 (\x y-> [x, y]) next next) xs

-- Another way to implement twoRandoms
twoRandoms' :: IO [Maybe Int]
twoRandoms' = (fst . runSupply (liftM2 (\x y-> [x, y]) next next)) `fmap` randomsIO
