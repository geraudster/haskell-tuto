doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                      then x
                      else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
conanO'Brien = "It's me, Conan O'Brien!"
boomBang xs = [ if x < 10 then "BOOM!" else "BANG!"  | x <- xs, odd x]
boomBang' xs fun = [ if x < 10 then "BOOM!" else "BANG!"  | x <- xs, fun x]
length' xs = sum [1 | _ <- xs]

removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree a b c = a+b+c

