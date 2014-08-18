solve :: Integer -> Integer
solve cuts = x * y
  where x = cuts `div` 2
        y = cuts - x

main = do
  _ <- getLine
  interact $ unlines . map (show . solve . read) . lines