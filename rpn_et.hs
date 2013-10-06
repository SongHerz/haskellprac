import Data.List

solveRPN :: String -> Maybe Float
solveRPN expression = 
    case foldl foldingFunction (Just []) . words $ expression of
        Just [x] -> Just x
        _        -> Nothing
    where   foldingFunction (Just ls) numberString =
                case (ls, numberString) of
                    ((x:y:ys), "*") -> Just $ (x * y):ys
                    ((x:y:ys), "+") -> Just $ (x + y):ys
                    ((x:y:ys), "-") -> Just $ (y - x):ys
                    ((x:y:ys), "/") -> Just $ (y / x):ys
                    ((x:y:ys), "^") -> Just $ (y ** x):ys
                    ((x:xs),  "ln") -> Just $ log x:xs
                    (xs,     "sum") -> Just $ [sum xs]
                    (xs, numberStr) -> let aNum = reads numberStr in
                                         case aNum of
                                            [(val, "")] -> Just $ val:xs
                                            _ -> Nothing
            foldingFunction Nothing _ = Nothing
