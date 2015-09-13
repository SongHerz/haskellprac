module MaybeState (
      MaybeState(..)
    , returnBad
    , evalMaybeState
    , execMaybeState
    , put
    , get) where


-- For this implementation,
-- when an error occurs, it is still possible to manipulate state.
-- Actually, this should be a composition of state monad and maybe monad.
-- (Am I right ?)
-- Go back to this problem after learning monad transformer.
newtype MaybeState s a = MaybeState {
    -- runstate :: MaybeState s a -> s -> (Maybe a, s)
    runState :: s -> (Maybe a, s)
}

evalMaybeState :: MaybeState s a -> s -> Maybe a
evalMaybeState m s =  let (v, _) = runState m s
                      in v

execMaybeState :: MaybeState s a -> s -> s
execMaybeState m s = let (_, s') = runState m s
                     in s'

-- Equivalent to 'return' of Monad
returnMaybeState :: a -> MaybeState s a
returnMaybeState a = MaybeState $ \s -> (Just a, s)

-- Equivalent to (>>=) of Monad
bindMaybeState :: MaybeState s a -> (a -> MaybeState s b) -> MaybeState s b
bindMaybeState m f = MaybeState $ \s -> let (a, s') = runState m s
                                        in case a of 
                                             Just v -> runState (f v) s'
                                             Nothing -> (Nothing, s')

-- Get current state
get :: MaybeState s s
get = MaybeState $ \s -> (Just s, s)

-- Alter current state
put :: s -> MaybeState s ()
put s = MaybeState $ \_ -> (Just (), s)

-- Do not use fail of a Monad, as its definition is inconsistent among
-- Monads.
returnBad :: MaybeState s a
returnBad = MaybeState $ \s -> (Nothing, s)

instance Monad (MaybeState s) where
    return = returnMaybeState
    (>>=) = bindMaybeState
