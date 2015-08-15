

doubleme x = x + x

doubleus x y = x*2 + y*2

doublesmall x = (if x < 100 then x*2 else x) + 1

sayMe :: (Integral a) => a -> String

sayMe 1 = "I'm a one!"
sayMe 2 = "I'm a two"
sayMe x = "I'm nothing special"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1+x2, y1+y2)

head' :: [a]->a
head' [] = error "Empty list, stupid!"
head' (x:_) = x

length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs  

-- @ symbol means bind to entire pattern
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  

--Guard functions
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi
    | bmi <= 18.0 = "You're a light weight"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"

max' :: (Ord a) => a->a->a
max' a b
    | a> b = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT  

initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname    

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs 

maximum2 :: (Ord a) => [a] -> a  
maximum2 [] = error "maximum of empty list"  
maximum2 [x] = x  
maximum2 (x:xs) = max' x (maximum2 xs)

index' :: (Integral n, Ord n) => n -> [a] -> a
index' n [] = error "Empty list"
index' n x
    | n < 0 = error "Negative"
    | n >= length' x = error "Overflow"
index' 0 x = head x
index' n (x:xs) = index' (n-1) xs

repeat' :: a -> [a]  
repeat' x = x:repeat' x 

take' :: (Num i, Ord i)=>i -> [a] -> [a]
take' n _
    |n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- a function and two arguments returns a new list
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- where clause`
largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0 

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n * 3 + 1)

midChain :: (Integral a) => a -> a -> a -> [a]
midChain c min max = filter f (chain c)
    where f x = x > min && x < max

sum' :: Num(a) => [a] -> a
sum' = foldl (+) 0

data Point = Point Float Float deriving Show
data Shape = Circle Point Float | Rectangle Point Point deriving Show

area :: Shape -> Float
area (Circle  _ r) = 3.14 * r^2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (x2-x1) * (y2-y1)

data Car = Car {
    make :: String
    , model :: String
    , year :: Int
} deriving Show

tellCar :: Car -> String
tellCar (Car {make=a, model=b, year=y}) = "This " ++ a ++ " " ++ b ++ " was made in " ++ show y


--rle_ :: (Eq a) => [[a]] -> [(Int, a)]    
--le_ [] = []
--le_ (x:xs) = (length x, head x) : rle_ xs

--le :: (Eq a) => [a] -> [(Int, a)]    
--le x = rle_ (group x)

-- Last item in a list
last' :: [a]->a
last' [] = error "Empty list"
last' [x] = x
last' (x:xs) = last' xs

-- sum items in a list
sum_' :: [Integer] -> Integer -> Integer
sum_' [x] y = x+y
sum_' (x:xs) y = sum_' xs y+x

sum1' :: [Integer] -> Integer
sum1' [] = error "Empty list"
sum1' xs = sum_' xs 0

-- All items except the last
init' :: [a]->[a]
init' [] = error "Empty list"
init' (x:y:[]) = [x]
init' (x:xs) = x : init' xs

--and' :: [Bool] -> Bool
--and' True = True
--and' (x:xs) = x : and' xs 

-- Given an int and a list, return a list of items divisible
-- by that int
returnDivisible :: Int -> [Int] -> [Int]
returnDivisible n xs = [x | x<-xs, (mod x n) == 0]

-- From a list of lists, return a list of the last item of
-- each list
tails :: [[Int]]->[Int]
tails xss = [last xs | xs <- xss]

-- Declare some data types
type Month = Int
type Day = Int
type Year = Int
type Name = String

data Date = Date Month Day Year
data Anniversary = Birthday Name Date
                | Wedding Name Name Date

showDate :: Date -> String
showDate (Date month day year) = show month ++ "/" ++ show day ++ "/" ++ show year

showAnniversary :: Anniversary -> String
showAnniversary (Wedding name1 name2 date) = 
    name1 ++ " married to " ++ name2 ++ " on " ++ showDate date
showAnniversary (Birthday name date) = 
    name ++ "'s birthday is on " ++ showDate date
