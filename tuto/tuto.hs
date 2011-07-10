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