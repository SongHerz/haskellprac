module Supply
(
  Supply
, next
, runSupply
) where

import Control.Monad.State

newtype Supply s a = S (State [s] a)

unwrapS :: Supply s a -> State [s] a
unwrapS (S s) = s

instance Monad (Supply s) where
    s >>= m = S (unwrapS s >>= unwrapS . m)
    return = S . return

next :: Supply s (Maybe s)
next = S $ do
    st <- get
    case st of
        [] -> return Nothing
        (x:xs) -> do
                    put xs
                    return (Just x)

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs

