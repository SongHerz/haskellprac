import Data.List
import Data.Char


bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          ( skinny, normal, fat) = ( 18.5, 25.0, 30.0)


calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [ bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

calcBmis' :: (RealFloat a) => [(a,a)] -> [a]
calcBmis' xs = [ bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25]


initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where f:_ = firstname
          l:_ = lastname


cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r *h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea



head' :: [a] -> a
head' [] = error "No head for an empty list!"
head' (x:_) = x


head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for an empty list!"
                       x:_ -> x

head''' :: [a] -> a
head''' = foldr1 (\x _ -> x)

head'''' :: [a] -> a
head'''' = foldl1 (\acc _ -> acc)


last' :: [a] -> a
last' = foldr1 (\_ acc -> acc)

last'' :: [a] -> a
last'' = foldl1 (\acc x -> x)


describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."


describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what :: [a] -> String
          what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list"



maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs


maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = let maxTail = maximum'' xs
                   in if x > maxTail then x else maxTail

maximum''' :: (Ord a) => [a] -> a
maximum''' = foldr1 (\x acc -> if x > acc then x else acc)


replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n - 1) x


take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0    = []
take' _ []      = []
take' n (x:xs)  = x : take' (n - 1) xs


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []


repeat' :: a -> [a]
repeat' x = x : repeat' x


zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys


elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs


elem'' :: ( Eq a) => a -> [a] -> Bool
elem'' y ys = foldl (\acc x -> if x == y then True else acc) False ys


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [ a | a <- xs, a <= x]
        biggerSorted = quicksort [ a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = smallerSorted ++ [x] ++ biggerSorted
    where smallerSorted = quicksort' ( filter (<=x) xs)
          biggerSorted = quicksort' ( filter ( > x) xs)


addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

addThree' :: (Num a) => a -> a -> a -> a
addThree' = \x -> \y -> \z -> x + y + z

multiThree :: (Num a) => a -> a -> a -> a
multiThree x y z = x * y * z


divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])


applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

flip'' :: ( a -> b -> c) -> ( b -> a -> c)
flip'' f = \x y -> f y x


filter' :: ( a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []


largestDivisible :: (Integral a) => a
largestDivisible = head ( filter p [100000, 99999 ..])
    where p x = x `mod` 3829 == 0


chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n : chain ( n `div` 2)
    | odd n = n : chain ( n * 3 + 1)


numLongChains :: Int
numLongChains = length ( filter (\xs -> length xs > 15) ( map chain [1 .. 100]))


sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: ( Num a) => [a] -> a
sum'' = foldl (+) 0


map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs


product' :: (Num a) => [a] -> a
product' = foldr1 (*)


sqrtSums :: Int
sqrtSums = length ( takeWhile ( < 1000) ( scanl1 (+) ( map sqrt [1..]))) + 1



ccencode :: Int -> String -> String
ccencode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in map chr shifted


ccdecode :: Int -> String -> String
ccdecode shift msg = ccencode ( negate shift) msg


findKey :: (Eq k) = k -> [(k,v)] -> v
findKey key xs = snd . head . filter ( \(k,v) -> key ==
