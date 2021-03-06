import Data.List
import System.Random
import State
import Control.Monad (liftM, liftM2)

rand :: IO Int
rand = getStdRandom $ randomR (0, maxBound)

twoBadRandoms :: (RandomGen g) => g -> (Int, Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)

twoGoodRandoms :: (RandomGen g) => g -> ((Int, Int), g)
twoGoodRandoms g = let (a, g') = random g
                       (b, g'') = random g'
                   in ((a, b), g'')

type RandomState a = State StdGen a

{-
- Someone has discussed this function:
- http://stackoverflow.com/questions/2574827/the-reason-for-monadstate-get-and-put
- The function can be rewritten as:
- getRandom = State $ \s -> random s
-
- From the answer and my thinking, the above implementation is not good,
- because it is not generic.
- For RandomState it is coincidence that the random returns (a, StdGen),
- and it fits \s -> (a, s).
- If random returns (some other value we do not care, a, StdGen), we must
- use the following method to implement getRandom.
-}
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

runTwoRandoms :: IO (Int, Int)
runTwoRandoms = do
    gen <- getStdGen
    let (v, gen') = runState getTwoRandoms gen
    setStdGen gen'
    return v

{-
- EXAMPLE ON MULTIPLE PIECES OF STATES
-}
-- Maintain multiple pieces of states
data CountedRandom = CountedRandom {
      crGen :: StdGen
      , crCount :: Int
      } deriving (Show)

type CRState = State CountedRandom

getCountedRandom :: Random a => CRState a
getCountedRandom = do
    st <- get
    let (v, gen') = random $ crGen st
    -- This changes all fields of a state
    -- And can also create a new state
    -- put $ CountedRandom { crGen = gen', crCount = crCount st + 1 }
    put st { crGen = gen', crCount = crCount st + 1 }
    return v

getCount :: CRState Int
getCount = crCount `liftM` get

putCount :: Int -> CRState ()
putCount a = do
    st <- get
    put st { crCount = a}

--
-- Test code on CountedRandom
--

-- Initialize a CountedRandom state with global stdGen
initCountedRandom :: IO CountedRandom
initCountedRandom = do
    gen <- getStdGen
    return CountedRandom {
          crGen = gen
        , crCount = 0 }

-- Give an initial CountedRandom,
-- return an infinite list of (Int, CountedRandom)
countedRandomSeries :: CountedRandom -> [(Int, CountedRandom)]
countedRandomSeries cr = let (v, cr') = runState getCountedRandom cr
                         in (v, cr') : countedRandomSeries cr'

get5Random :: CountedRandom -> [(Int, CountedRandom)]
get5Random cr = take 5 $ countedRandomSeries cr

g5RPC11g5R :: CountedRandom -> [(Int, CountedRandom)]
g5RPC11g5R cr = concat [_5r0, [(c, cr'')], _5r1]
    where _5r0 = get5Random cr
          (c, cr') = last _5r0
          cr'' = execState (putCount 11) cr'
          _5r1 = get5Random cr''

testCountedRandom :: IO ()
testCountedRandom = do
    counted_random <- initCountedRandom
    let cr_list = g5RPC11g5R counted_random
    putStrLn $ intercalate "\n" $ map show cr_list
