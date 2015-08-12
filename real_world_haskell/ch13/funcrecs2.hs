data FuncRec = 
    FuncRec { name :: String
            , calc :: Int -> Int
            , namedCalc :: Int -> (String, Int)}

mkFuncRec :: String -> (Int -> Int) -> FuncRec
mkFuncRec name calcFunc =
    FuncRec { name = name
            , calc = calcFunc
            , namedCalc = \x -> (name, calcFunc x)}

plus5 = mkFuncRec "plus5" (+5)
alway0 = mkFuncRec "always0" (\_ -> 0)
