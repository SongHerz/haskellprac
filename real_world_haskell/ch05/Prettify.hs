module Prettify
    (
      Doc,
      (<>),
      char,
      double,
      text,
      fsep,
      hcat,
      punctuate,
      compact,
      pretty,
      fill
    ) where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show,Eq)


empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y


hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []      = []
punctuate p [d]     = [d]
punctuate p (d:ds)  = (d <> p) : punctuate p ds

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y)  = flatten x `Concat` flatten y
flatten Line            = Char ' '
flatten (x `Union` _)   = flatten x
flatten other           = other

compact :: Doc -> String
compact x = transform [x]
    where transform []      = ""
          transform (d:ds)  =
              case d of
                Empty           -> transform ds
                Char c          -> c : transform ds
                Text s          -> s ++ transform ds
                Line            -> '\n' : transform ds
                a `Concat` b    -> transform (a:b:ds)
                _ `Union` b     -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                Empty           -> best col ds
                Char c          -> c : best (col + 1) ds
                Text s          -> s ++ best (col + length s) ds
                Line            -> '\n' : best 0 ds
                a `Concat` b    -> best col (a:b:ds)
                a `Union` b     -> nicest col (best col (a:ds))
                                              (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col


fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w-1) `fits` cs

-- ex1 on page 130
-- I think, the meaning of the exercise is to find the longest line of a Doc,
-- and append spaces to that line, if the line is shorter than required.
-- Assume all new lines are represented by Line, and no char or text contains new line.
fill :: Int -> Doc -> Doc
fill width Empty    = Empty
fill width x        = if longestWidth >= width || remainWidth >= width
                         then x
                         else if remainWidth >=longestWidth
                              then x <> text (replicate (width - remainWidth) ' ')
                              else replaceWithLongest x ds
                      where (_, longestWidth, remainWidth, ds) = longestLine x
                            replaceWithLongest Line [] = text (replicate (width - longestWidth) ' ') <> Line
                            replaceWithLongest _    [] = error "Should not be here"
                            replaceWithLongest d (ad : ds) =
                                case d of
                                   a `Concat` b | ad == LEFT  -> replaceWithLongest a ds `Concat` b
                                                | ad == RIGHT -> a `Concat` replaceWithLongest b ds
                                   a `Union` b  | ad == RIGHT -> a `Union` (replaceWithLongest b ds)
                                                | ad == LEFT  -> error "Should not be here"
                                   _            -> error "Should not be here"

data ConcatBranch = LEFT | RIGHT
                    deriving (Show, Eq)

-- Given a Doc, return a tuple 
-- (The Document with the longest line
-- , number of chars in the longest line
-- , remaining chars that has not been terminated yet
-- , trace to the Line Doc)
-- When no Line found in the Doc, the 1st element of the tuple is Empty,
-- and the number of chars in the longest line is 0.
longestLine :: Doc -> (Doc, Int, Int, [ConcatBranch])
longestLine d = (ad, dw, rw, reverse ds)
    where (ad, dw, rw, ds) = helper 0 d []
          -- The helper function takes:
          -- number of remaining chars, a Doc, and a trace
          -- The number of remaining chars are on the left side of the Doc
          --         Concat Doc
          --            ^
          --      left  |
          --            |      left
          --        SomeDoc0 <------- SomeDoc1
          helper n Empty    ds        = (Empty, 0, n, ds)
          helper n (Char c) ds        = (Empty, 0, n + 1, ds)
          helper n (Text s) ds        = (Empty, 0, n + length s, ds)
          helper n Line     ds        = (Line,  n, 0, ds)
          helper n (a `Concat` b) ds  = fromConcat (da, na, nra, ads) (db, nb, nrb, bds)
              where (da, na, nra, ads) = helper n a (LEFT : ds)
                    (db, nb, nrb, bds) = helper (nra) b (RIGHT : ds)
                    fromConcat (Empty, _, _, _) (db, nb, nrb, bds) = (db, nb, nrb, bds)
                    fromConcat (da, na, _, ads) (Empty, _, nrb, _) = (da, na, nrb, ads)
                    fromConcat (da, na, _, ads) (db, nb, nrb, bds) = if na >= nb
                                                                     then (da, na, nrb, ads)
                                                                     else (db, nb, nrb, bds)
          helper n (_ `Union` b) ds   = helper n b (RIGHT : ds)
