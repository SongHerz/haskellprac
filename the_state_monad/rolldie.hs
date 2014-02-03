import System.Random

newtype State state result = State { runState :: state -> (result, state) }

instance Monad (State state_type) where
    --return :: result -> State state result
    return r = State (\s -> (r, s))

    --(>>=) :: State state result_a -> (result_a -> State state result_b) -> State state result_b
    processor >>= processorGenerator = State $ \state ->
                                        let (result, state') = runState processor state
                                        in runState (processorGenerator result) state'

put newState = State $ \_ -> ((), newState)
get = State $ \state -> (state, state)

evalState stateMonad state = fst (runState stateMonad state)
execState stateMonad state = snd (runState stateMonad state)

type GeneratorState = State StdGen


rollDie :: GeneratorState Int
rollDie = do
    generator <- get
    let ( value, newGenerator) = randomR (1,6) generator
    put newGenerator
    return value

--evalState rollDie (mkStdGen 0)


rollDie :: State StdGen Int
rollDie = get >>= (\generator ->
          let (value, newGenerator) = randomR(1,6) generator
          in put newGenerator >> return value)


rollDie :: State StdGen Int
rollDie = (State $ \state -> (state, state)) >>= (\gState ->
          let (value, gState') = randomR (1,6) gState
          in (State $ \_ -> ((), gState')) >> return value)

rollDie :: State StdGen Int
rollDie = (State $ \state -> (state, state)) >>= (\gState ->
          let (value, gState') = randomR (1,6) gState
          in (State $ \_ -> ((), gState')) >>= (\_ -> return value))

x >>= y >>= z

x = State $ \state -> (state, state)

y = \gState -> let (value, gState') = randomR (1,6) gState
               in (State $ \_ -> ((), gState'))

z = \_ -> return value

x >>= (\w -> y w >>= z)

\w -> (y w >>= z) = \w -> State $ \state ->
                          let (result, state') = runState (y w) state
                          in runState (z result) state'

\w -> (y w >>= z) = \w -> State $ \state ->
                          let (value, gState') = randomR (1,6) w
                              (result, state') = ((), gState')
                          in runState (z result) state'

\w -> (y w >>= z) = \w -> State $ \state ->
                          let (value, gState') = randomR (1,6) w
                              (result, state') = ((), gState')
                          in (value, state')

\w -> (y w >>= z) = \w -> State $ \_ -> randomR (1, 6) w



rollDie = get >>= (\generator ->
          let (value, newGenerator) = randomR (1,6) generator 
          in put newGenerator >> return value)


put newGenerator >> return value
put newGenerator >>= (\_ -> return value)

State $ (\_ -> ((), newGenerator)) >>= (\_ -> return value)

State $ \state ->
   let (result, state') = (\_ -> ((), newGenerator)) state
   in runState ((\_ -> return value) result) state'

State $ \state ->
   let (result, state') = ((), newGenerator)
   in runState (return value) state'

State $ \state ->
   let (result, state') = ((), newGenerator)
   in runState (State (\s -> (value, s))) state'

State $ \state ->
   let (result, state') = ((), newGenerator)
   in (value, state')

State $ \state ->
   let (result, state') = ((), newGenerator)
   in (value, newGenerator)

rollDie = get >>= (\w -> \generator ->
          let (value, newGenerator) = randomR (1,6) generator
          in (value, newGenerator)

rollDie = get >>= (\w -> \generator -> randomR (1,6) generator)


rollDie = get >>= f

get >>= f

State $ /state ->
    let (result, state') = runState get state
    in runState ( f result) state'

State $ /state ->
    let (result, state') = (state, state)
    in ( randomR (1,6) result) state'

rollDie = randomR (1,6) state
