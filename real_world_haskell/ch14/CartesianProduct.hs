import Control.Monad (liftM2)
comprehensive xs ys = [(x, y) | x <- xs, y <- ys]

monadic xs ys = do { x <- xs; y <- ys; return (x, y) }

blockDo xs ys = do
    x <- xs
    y <- ys
    return (x, y)

blockPlain xs ys =
    xs >>= \x ->
    ys >>= \y ->
    return (x, y)

blockLift xs ys = liftM2 (\x y -> (x, y)) xs ys

