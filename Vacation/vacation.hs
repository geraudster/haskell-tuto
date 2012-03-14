import Data.List
import Data.Char
import qualified Data.Map as Map

 -- cumul 3 2 => [2,4,6]
cumul :: (Num a) => Integer -> a -> [a]
cumul 0 _ = []
cumul count x =  (cumul (count - 1) x) ++ [fromInteger count * x]

calendarList = 
  [("Jan.", 0)
  ,("Feb.", 0)
  ,("Mar.", 0)
  ,("Apr.", 0)
  ,("May", 0)
  ,("Jun.", 0)
  ,("Jul.", 0)
  ,("Aug.", 0)
  ,("Sep.", 0)
  ,("Oct.", 0)
  ,("Nov.", 0)
  ,("Dec.", 0)
  ]
