import Control.Monad.Error

type Birds = Int
type Pole = (Birds, Birds)

errMsg :: Pole -> Pole -> String
errMsg (oldLeft, oldRight) (newLeft, newRight) =
    "There are " ++ (show oldLeft) ++ " birds on the left, "
    ++ (show oldRight) ++ " birds on the right. "
    ++ "Another " ++ (show ( if newLeft > oldLeft then newLeft - oldLeft else newRight - oldRight))
    ++ " birds on the " ++ ( if newLeft > oldLeft then "left" else "right") ++ " make the man fall."

{-
 -- This does not work
landLeft :: Error e => Birds -> Pole -> Either e Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Right (left + n, right)
    | otherwise                    = Left $ strMsg $ errMsg (left, right) (left + n, right)

landRight :: Error e => Birds -> Pole -> Either e Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Right (left, right + n)
    | otherwise                    = Left $ strMsg $ errMsg (left, right) (left, right + n)
-}


landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Right (left + n, right)
    | otherwise                    = Left $ errMsg (left, right) (left + n, right)

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Right (left, right + n)
    | otherwise                    = Left $ errMsg (left, right) (left, right + n)
