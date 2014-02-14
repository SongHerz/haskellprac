data BookInfo = Book Int String [String]
                deriving (Show)

data MagzineInfo = Magzine Int String [String]
                   deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

type CustomerID = Int
type ReviewBody = String

data BookReview = BookReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)
         