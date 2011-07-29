circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!!!"
lucky _ = "Sorry, you're out of luck :("

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = factorial (x-1) * x

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Bobby"
charName 'c' = "Childeric"
charName x = error ("Unknown Char " ++ show x)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1+x2, y1+y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "List is empty"
tell [x] = "List has one element " ++ show x
tell [x,y] = "List has two elements " ++ show x ++ " and " ++ show y
tell [x,y,_] = "List is too long..."

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= 18.5 = "Underweight"
  | bmi <= 25.0 = "Normal"
  | bmi <= 30.0 = "Fat"
  | otherwise = "Too fat"
    where bmi = weight / (height^2)

max' :: (Ord a) => a -> a -> a
max' x y
  | x < y = y
  | otherwise = x

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [ bmi w h | (w,h) <- xs ]
  where bmi weight height = weight / height^2

fibonacci :: (Num a) => a -> a
fibonacci n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fibonacci (n-1) + fibonacci (n-2) 

testtype :: [a] -> [[a]]
testtype x = [x]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

chain :: Int -> [Int]
chain x
  | x<=1 = [1]
  | even x = x : chain (x `div` 2)
  | otherwise = x : chain (x*3+1)
numLongChains :: Int
numLongChains = length (filter criteria (map chain [1..100]))
  where
    criteria xs = length xs > 15

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

sqrtSums :: Int
sqrtSums = length . takeWhile (<1000) . scanl1 (+) $ map sqrt [1..]

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector x1 y1 z1) `vplus` (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector x y z) `vmult` n = Vector (x*n) (y*n) (z*n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector x1 y1 z1) `dotProd` (Vector x2 y2 z2) = x1*x2 + y1*y2 + z1*z2