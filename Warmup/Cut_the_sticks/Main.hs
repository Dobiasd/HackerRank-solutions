import Control.Arrow
import Data.List
import Data.List.Split

(|>) a b = b a
infixl 0 |>

main :: IO ()
main = do
    _ <- getLine
    interact $ readInts >>> solve >>> map show >>> intercalate "\n"

solve :: [Integer] -> [Int]
solve = iterate step >>> map length >>> takeWhile (>0)

step :: [Integer] -> [Integer]
step xs = map (subtract (minimum xs)) xs |> filter (>0)

readInts :: String -> [Integer]
readInts = splitOn " " >>> map read