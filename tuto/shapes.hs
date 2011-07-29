module Shapes
       ( Point(..)
       , Shape(..)
       , area
       , perimeter
       , baseCircle
       , baseRect
       , nudge
) where
-- data type / shape tests
-- Circle xc yc r : center (xc, yc), radius (r)
-- Rectangle x1 y1 x2 y2  : upper left point (x1, y1) lower right point (x2, y2)
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

distance :: Point -> Point -> Float
distance (Point x1 y1) (Point x2 y2) = sqrt $ (x1-x2) ^ 2 + (y1-y2) ^2

perimeter :: Shape -> Float
perimeter (Circle _ r) = 2 * pi * r
perimeter (Rectangle (Point x1 y1) (Point x2 y2)) = (distance (Point x1 y1) (Point x1 y2)) + (distance (Point x1 y1) (Point x2 y1)) 

shapeType :: Shape -> String
shapeType (Circle _ _) = "Circle"
shapeType (Rectangle _ _) = "Rectangle"

origin = Point 0 0
baseCircle :: Float -> Shape
baseCircle r = Circle origin r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle origin (Point width height) 

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))
