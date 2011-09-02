import Control.Monad

main = do
  line <- getLine
  when (not . null $ line) $ do
    putStrLn (reverseWords line)
    main

reverseWords = unwords . map reverse . words