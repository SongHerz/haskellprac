data Maybe' a = Just' a
              | Nothing'
              deriving (Show)

someBool = Just' True
someString = Just' "something"
wrapped = Just' (Just' "wrapped")
