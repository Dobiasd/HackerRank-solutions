import Control.Arrow
import Data.List.Split

(|>) a b = b a
infixl 0 |>

main :: IO ()
main = do
    _ <- getLine
    interact $ lines >>> map (readInts >>> solve >>> show) >>> unlines

readInts :: String -> [Integer]
readInts = splitOn " " >>> map read

solve :: [Integer] -> Int
solve [a, b] = length squares
    where intSqrt = fromIntegral >>> sqrt >>> floor
          nStart = intSqrt a
          nEnd = intSqrt b
          squares = map (^2) [nStart..nEnd] |> filter (\x -> x >= a && x <= b)