import Control.Arrow

main :: IO ()
main = interact $ lines >>> tail >>> map (read >>> solve >>> show) >>> unlines

solve :: Integer -> Integer
solve = (+1)