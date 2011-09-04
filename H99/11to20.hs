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

-- Problem 11 
data Record a = Single a | Multiple Int a deriving (Show)
encodeModified :: (Eq a) => [a] -> [Record a]
encodeModified [] = []
encodeModified xs = [ (getRecord (length all) xx) | all@(xx:xxs) <- pack xs ]
  where
    getRecord 1 value = Single value
    getRecord count value = Multiple count value

-- Problem 12
decodeModified :: (Eq a) => [Record a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = (decodeRecord x) ++ decodeModified xs
  where
    decodeRecord (Single value) = [value]
    decodeRecord (Multiple count value) = replicate count value

-- Problem 13
encodeDirect :: (Eq a) => [a] -> [Record a]
encodeDirect [] = []
encodeDirect (x:xs) = encode' xs (Single x)
  where
    encode' [] record  = [record]
    encode' (x:xs) record
      | x == (valueOf record) = encode' xs (incRecord record)
      | otherwise = record : encode' xs (Single x)
    incRecord (Single value) = (Multiple 2 value)
    incRecord (Multiple count value) = (Multiple (count+1) value)
    valueOf (Single value) = value
    valueOf (Multiple _ value) = value

    
-- Problem 14
duplicate :: [a] -> [a]
duplicate = foldr (\x acc -> x:x:acc) []

-- Problem 15
repli :: [a] -> Int -> [a]
repli l n = foldl (\acc x -> acc ++ (replicate n x)) [] l 

repli' =  flip $concatMap . replicate

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery l n = drop' l 1 
  where drop' [] _ = []
        drop' (x:xs) m 
          | (m `mod` n)  == 0 = drop' xs 1
          | otherwise = x:drop' xs (m+1)

-- Problem 17
split :: [a] -> Int -> [[a]]
split l n = helper l 0 []
  where helper [] _ acc = [acc]
        helper (x:xs) count acc
          | count == n = [acc,x:xs]
          | otherwise = helper xs (count+1) (acc++[x])

-- split "abcdefgh" 3
-- ["abc","defgh"]
split' :: [a] -> Int -> ([a],[a])
split' l 0 = ([],l)
split' [] _ = ([],[])
split' (x:xs) n = let (l1,l2) = split' xs (n-1) in (x:l1,l2)