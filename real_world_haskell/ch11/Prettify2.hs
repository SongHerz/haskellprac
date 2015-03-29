import Test.QuickCheck

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Eq, Show)


data Ternary = Yes
             | No
             | Unknown
             deriving (Eq, Show)

instance Arbitrary Ternary where
    arbitrary = elements [Yes, No, Unknown]

newtype Ternary2 = Ternary2 Ternary
                   deriving (Eq, Show)

instance Arbitrary Ternary2 where
    arbitrary = do
        n <- choose (0, 2) :: Gen Int
        return $ case n of
                     0 -> Ternary2 Yes
                     1 -> Ternary2 No
                     _ -> Ternary2 Unknown

-- instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
--     arbitrary = do
--         x <- arbitrary
--         y <- arbitrary
--         return (x, y)

main = do
    -- Show Ternary lists
    verboseCheck ((\_ -> True) :: [Ternary] -> Bool)
    -- Show Ternary2 lists
    verboseCheck ((\_ -> True) :: [Ternary2] -> Bool)


        
