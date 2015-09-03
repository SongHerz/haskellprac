module State (
      State(..)
    , put
    , get) where


newtype State s a = State {
    runState :: s -> (a, s)
}

-- Equivalent to 'return' of Monad
returnState :: a -> State s a
returnState a = State $ \s -> (a, s)

-- Equivalent to (>>=) of Monad
bindState :: State s a -> (a -> State s b) -> State s b
bindState m f = State $ \s -> let (a, s') = runState m s
                                  in runState (f a) s'

-- Get current state
get :: State s s
get = State $ \s -> (s, s)

-- Alter current state
put :: s -> State s ()
put s = State $ \_ -> ((), s)

instance Monad (State s) where
    return = returnState
    (>>=) = bindState
