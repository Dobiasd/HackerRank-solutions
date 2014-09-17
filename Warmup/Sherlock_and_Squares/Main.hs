import Control.Arrow
import Data.List.Split

(|>) :: a -> (a -> b) -> b
(|>) x y = y x
infixl 0 |>

main :: IO ()
main = do
    _ <- getLine
    interact $ lines >>> map (readInts >>> solve >>> show) >>> unlines

readInts :: String -> [Integer]
readInts = splitOn " " >>> map read

solve :: [Integer] -> Int
solve [a, b] = length squares
    where intSqrt = fromIntegral >>> (sqrt :: Double -> Double) >>> floor
          nStart = intSqrt a
          nEnd = intSqrt b
          squares = map (^(2 :: Integer)) [nStart..nEnd]
            |> filter (\x -> x >= a && x <= b)
solve _ = error "list must have length 2"