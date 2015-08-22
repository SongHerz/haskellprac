module Logger (
      Log
    , Logger
    , record
    , runLogger) where

type Log = [String]

newtype Logger a = Logger { execLogger :: (a, Log) }

runLogger = execLogger

record s = Logger ((), [s])

instance Monad Logger where
    -- Type signature in instance definition cause error.
    -- At least, GHC 7.8.3 does not allow this.
    -- return :: a -> Logger a
    return a = Logger (a, [])

    -- (>>=) :: Logger a -> (a -> Logger b) -> Logger b
    (>>=) ma f = let (va, la) = execLogger ma
                     (vb, lb) = execLogger $ f va
                 in Logger (vb, la ++ lb)
