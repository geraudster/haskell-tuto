import Data.List
import Data.Char
import qualified Data.Map as Map

 -- needle C haystack
isInHaystack :: (Eq a) => [a] -> [a] -> Bool
isInHaystack needle  = any (\xs -> needle `isPrefixOf` xs) . tails

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (\xs -> needle `isPrefixOf` xs) (tails haystack)

 -- cipher
encode offset msg = map (\x -> chr $ ord x + offset) msg
decode offset msg = encode (negate offset) msg

 -- sum of digit number equal to 42
digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstNumber = head [ x | x <- [0,1..], digitSum x == 42 ]

phoneBook =
  [("tutu", "12.34.56.78.90")
  ,("toto", "01 11 11 11 11")
  ,("tata", "0311223344")
  ,("tata", "01 12 34 56 78")
  ]

phoneBookMap :: Map.Map String String
phoneBookMap = phoneBookToMap phoneBook

phoneBookMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookMap' xs = Map.fromListWith (++) $ map (\(k,v) -> (k, [v])) xs

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit 

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
  where add number1 number2 = number1 ++ ", " ++ number2