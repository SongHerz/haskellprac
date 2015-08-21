-- Some code to revisit the methology introduced by ch10.
-- Hope I can write it myself without referring ch10.
-- The key:
-- 1. The next operation is in the scope of the operator function.
-- 2. Pass state implicitly by functions.
-- 
-- AHHH, I cheated, I referred code of ch10 for
-- Consume a
-- ==>
-- ==&
-- :-(
{-# LANGUAGE ScopedTypeVariables #-}

data State = State { getState :: String }

data Consume a = Consume { runConsume :: State -> Maybe (a, State) }

identity :: a -> Consume a
identity a = Consume $ \s -> Just (a, s)

(==>) :: forall a b. Consume a -> (a -> Consume b) -> Consume b
(==>) ca f = Consume chain
    where chain :: State -> Maybe (b, State)
          chain s = case runConsume ca s of
                        Nothing -> Nothing
                        Just (va, new_st) -> runConsume (f va) new_st

(==&) :: forall a b. Consume a -> Consume b -> Consume b
-- NOTE: MUST use ==> to implement, becuase action from ca must be
-- executed, and this action may alter the state.
(==&) ca cb = ca ==> \_ -> cb


