-- Some code to revisit the methology introduced by ch10.
-- Hope I can write it myself without referring ch10.
-- The key:
-- 1. The next operation is in the scope of the operator function.
-- 2. Pass state implicitly by functions.
-- 3. Each function is an action, and ==> and ==& compose actions to a new
-- action.
-- 4. To get actual result from input, just apply an action to the input.
-- 
-- AHHH, I cheated, I referred code of ch10 for
-- Consume a
-- ==>
-- ==&
-- :-(
-- But rest code is written by me :-)
-- putState and getState is used in ch10 for some primary operations,
-- but I compose new state explicitly here.
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Char (isSpace, isDigit)

data State = State { getString :: String }

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


skipSpaces :: Consume ()
skipSpaces = Consume helper
    where helper :: State -> Maybe ((), State)
          helper s = let str = getString s
                         (_, non_spcs) = span isSpace str
                         in Just ((), s {getString = non_spcs})

getInt :: Consume Int
getInt = Consume helper
    where helper :: State -> Maybe (Int, State)
          helper s = let str = getString s
                         (digits, rest) = span isDigit str
                         in case digits of
                                [] -> Nothing
                                _  -> Just (read digits, s {getString = rest})

getX :: Consume Char
getX = Consume helper
    where helper :: State -> Maybe (Char, State)
          helper s = let str = getString s
                         in case str of
                                'x' : rest -> Just ('x', s {getString = rest})
                                'X' : rest -> Just ('X', s {getString = rest})
                                _ -> Nothing

-- Parse a string like "123 x 456", return (123, 456)
parseAction :: Consume (Int, Int)
parseAction = skipSpaces ==&
         getInt ==> \d0 -> skipSpaces ==&
         getX ==> \x -> skipSpaces ==&
         getInt ==> \d1 -> identity (d0, d1)

parse :: String -> Maybe (Int, Int)
parse s = case runConsume parseAction $ State s of
              Nothing -> Nothing
              Just (v, _) -> Just v

r0 = parse "123"
r1 = parse "123x456"
r2 = parse "   456   X 324  "

              
