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