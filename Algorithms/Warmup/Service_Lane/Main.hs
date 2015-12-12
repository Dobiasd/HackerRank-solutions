import Control.Applicative
import Control.Arrow
import Data.List.Split

(|>) :: a -> (a -> b) -> b
(|>) x y = y x
infixl 0 |>

main :: IO ()
main = do
    _ <- getLine
    widths <- readInts <$> getLine
    interact $ lines >>> map (readInts >>> solve widths >>> show) >>> unlines

readInts :: String -> [Int]
readInts = splitOn " " >>> map read

solve :: [Int] -> [Int] -> Int
solve widths [i, j] = sublist i (j + 1) widths |> minimum
solve _ _ = error "wrong number of inputs, 2 expected"

-- right open interval
sublist :: Int -> Int -> [a] -> [a]
sublist start end = drop start >>> take (end - start)