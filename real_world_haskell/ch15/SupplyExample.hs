import Supply
import Control.Monad

example0 = runSupply next [1,2,3]
example1 = runSupply (liftM2 (,) next next) [1]
