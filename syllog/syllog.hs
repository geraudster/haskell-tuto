data Qualite = X | Y | X' | Y' | Qualite Qualite Qualite
             deriving(Show)

data BiLitteral = BiLitteral (Maybe Bool) (Maybe Bool) (Maybe Bool) (Maybe Bool)
                  deriving(Show)
sampleOne :: BiLitteral
sampleOne = BiLitteral Nothing Nothing Nothing Nothing
sampleTwo = BiLitteral (Just True) Nothing Nothing Nothing
sampleThree = BiLitteral (Just False) Nothing Nothing Nothing


interpret (BiLitteral a b c d) = map toText $ [a, b, c, d]
  where toText x
          | x == (Just True) = "some x are y"
          | x == (Just False) = "no x are y"
          | otherwise = ""
                                       
