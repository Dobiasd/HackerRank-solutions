import Data.List.Split
import Control.Arrow

(|>) x y = y x
infixl 0 |>

main :: IO ()
main = do
  _ <- getLine
  interact $ lines >>> filter (not . null) >>>
             map (splitOn " " >>> map read >>> solve >>> show) >>> unlines

solve :: [Int] -> Int
solve [n, c, m] = iterate f (n, c, m, 0, 0)
                  |> converge (==)
                  |> \ (n, c, m, w, r) -> r

f (n, c, m, w, r) = (n', c, m, w' + r'', r + r'')
  where (r',  n') = n `divMod` c
        (rw', w') = w `divMod` m
        r'' = r' + rw'

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys