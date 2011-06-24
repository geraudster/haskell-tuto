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


