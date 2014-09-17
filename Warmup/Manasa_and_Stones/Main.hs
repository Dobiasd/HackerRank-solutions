import Control.Arrow
import Data.List
import Data.List.Split

(|>) :: a -> (a -> b) -> b
(|>) x y = y x
infixl 0 |>

main :: IO ()
main = do
    _ <- getLine
    interact $ lines >>> chunksOf 3 >>>
                map (map read >>> solve >>> map show >>> unwords) >>>
                unlines

solve :: [Int] -> [Int]
solve [n, a, b] = applyNTimes (nextComb [a, b] >>> sort >>>
                                        (sort >>> group >>> map head))
                              (n - 1) [0]
solve _ = error "wrong number of items"

applyNTimes :: (a -> a) -> Int -> a -> a
applyNTimes f n = foldr (.) id (replicate n f)

nextComb :: [Int] -> [Int] -> [Int]
nextComb summands xs = do
    x <- xs
    summand <- summands
    [x + summand]