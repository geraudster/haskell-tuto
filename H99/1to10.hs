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
myReverse' [] = []
myReverse' (x:xs) = myReverse' xs ++ [x] 

myReverse'' :: [a] -> [a]
myReverse'' list = reverse''' list []
  where
    reverse''' [] acc = acc
    reverse''' (x:xs) acc = reverse''' xs (x:acc)

 -- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = list == reverse list

 -- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten list = flatten' list []
  where
    flatten' (List []) acc  = acc
    flatten' (Elem x) acc = x:acc
    flatten' (List (x:xs)) acc = flatten' x (flatten' (List xs) acc)

 -- Problem 8
    -- (compress '(a a a a b c c a a d e e e e))
    --  => (A B C A D E)
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = compress' x xs
  where compress' current [] = [current]
        compress' current (next:list)
          | current == next = compress' current list
          | otherwise = current : compress' next list
    

 -- Problem 9
        -- * (pack '(a a a a b c c a a d e e e e))
        -- ((A A A A) (B) (C C) (A A) (D) (E E E E))
pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack (x:xs) = pack' x xs [] []
  where pack' current [] current_pack packed = packed ++ [current:current_pack]
        pack' current (next:list) current_pack packed
          | current == next = pack' current list (current:current_pack) packed 
          | otherwise = pack' next list [] (packed ++ [current:current_pack])

 -- Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode xs = [ (length all, xx) | all@(xx:xxs) <- pack xs ]
  