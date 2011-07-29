module Persons 
       ( Person
       )where

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Int
                     , phoneNumber :: String
                     , flavor :: String } deriving (Show)
