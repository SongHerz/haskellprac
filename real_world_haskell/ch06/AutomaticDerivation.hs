data CannotShow = CannotShow
                deriving (Show)

-- will not compile, since CannotShow is not an instance of Show
-- NOTE: To make compilation fail, the 'deriving (Show)' of CannotShow
-- should be commented out or deleted.
data CannotDeriveShow = CannotDeriveShow CannotShow
                        deriving (Show)


data OK = OK

instance Show OK where
    show _ = "OK"

data ThisWorks = ThisWorks OK
                 deriving (Show)
