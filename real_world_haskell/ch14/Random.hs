import System.Random
import State
import Control.Monad (liftM2)

rand :: IO Int
rand = getStdRandom $ randomR (0, maxBound)

twoBadRandoms :: (RandomGen g) => g -> (Int, Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)

twoGoodRandoms :: (RandomGen g) => g -> ((Int, Int), g)
twoGoodRandoms g = let (a, g') = random g
                       (b, g'') = random g'
                   in ((a, b), g'')

type RandomState a = State StdGen a

getRandom :: Random a => RandomState a
getRandom =
    get >>= \gen ->
    let (val, gen') = random gen in
        put gen' >>
        return val

getRandom' :: Random a => RandomState a
getRandom' = do
    gen <- get
    let (val, gen') = random gen
    put gen'
    return val

getTwoRandoms :: Random a => RandomState (a, a)
getTwoRandoms = liftM2 (,) getRandom getRandom
