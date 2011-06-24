 -- Problem 1
myLast = last

myLast' :: [a] -> a
myLast' [x] = x
myLast' (_:xs) = myLast' xs

 -- Problem 2
myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

 -- Problem 3
elementAt xs n = xs !! (n-1)

elementAt' :: (Num n) => [a] -> n -> a
elementAt' (x:_) 1 = x
elementAt' (_:xs) n = elementAt' xs (n-1)
elementAt' _ _ = error "Index out of bounds"

 -- Problem 4
myLength = length

myLength' :: (Num n) => [a] -> n
myLength' [] = 0
myLength' (_:xs) = 1 + myLength' xs

myLength'' xs = sum [1 | _ <- xs]

 -- Problem 5
myReverse = reverse

myReverse' :: [a] -> [a]
myReverse' (x:xs) = myReverse' xs ++ [x] 
myReverse' _ = []

myReverse'' :: [a] -> [a]
myReverse'' list = reverse''' list []
  where
    reverse''' [] acc = acc
    reverse''' (x:xs) acc = reverse''' xs (x:acc)
