import Data.Char

main = do
  putStrLn "Your firstname?"
  firstname <- getLine
  putStrLn "Your lastname?"
  lastname <- getLine
  let bigfirstname = map toUpper firstname
      biglastname = map toUpper lastname
  putStrLn ("hey " ++ bigfirstname ++ " " ++ biglastname)

  