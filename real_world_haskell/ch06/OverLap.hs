{-# LANGUAGE FlexibleInstances #-}
{-
This is intended to fail, when invoking 'bork (1,2)',
because both instances 'Borked (Int, Int)' and 'Borked (a, b)' matches.
To match 'Borked (Int, Int)' only, 'OverlappingInstances' extension is required.
-}

class Borked a where
    bork :: a -> String

instance Borked Int where
    bork = show

instance Borked (Int, Int) where
    bork (a, b) = bork a ++ ", " ++ bork b

instance (Borked a, Borked b) => Borked (a, b) where
    bork (a, b) = ">>" ++ bork a ++ " " ++ bork b ++ "<<"
