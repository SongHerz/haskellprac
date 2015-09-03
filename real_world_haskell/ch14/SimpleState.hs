type SimpleState s a = s -> (a, s)

type StringState a = SimpleState String a

-- This is not for returning state of SimpleState var,
-- This is 'return' for SimpleState type.
returnSt :: a -> SimpleState s a
returnSt a = \s -> (a, s)

-- An alternative version of returnSt,
-- Just expand 'SimpleState s a' and look at the type signature of
-- this function.
-- returnAlt a <==> returnSt a
returnAlt :: a -> SimpleState s a
returnAlt a s = (a, s)

-- (>>=) of Monad
bindSt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
bindSt m f = \s -> let (a, s') = m s
                       in (f a) s'

-- Get state from SimpleState.
-- This is different from returnSt.
getSt :: SimpleState s s
getSt = \s -> (s, s)

-- Put state to SimpleState
putSt :: s -> SimpleState s ()
putSt s = \_ -> ((), s)
