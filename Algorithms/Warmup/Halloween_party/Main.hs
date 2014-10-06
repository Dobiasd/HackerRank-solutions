import Control.Arrow

solve :: Integer -> Integer
solve cuts = x * y
  where x = cuts `div` 2
        y = cuts - x

main :: IO ()
main = do
  _ <- getLine
  interact $ lines >>> map (read >>> solve >>> show) >>> unlines