import Control.Arrow
import Control.Applicative
import Data.List.Split

(|>) :: a -> (a -> b) -> b
(|>) x y = y x
infixl 0 |>

main :: IO ()
main = do
    n:_  <- readInts <$> getLine
    interact $ lines >>> map readInts >>> solve n >>> show >>> (++"\n")

readInts :: String -> [Integer]
readInts = splitOn " " >>> map read

solve :: Integer -> [[Integer]] -> Integer
solve n commands = foldl (\x f -> f x) 0 fs |> (`div` n)
    where fs = map commandToFunc commands

commandToFunc :: [Integer] -> Integer -> Integer
commandToFunc [a, b, k] = \x -> x + (((b - a) + 1) * k)
commandToFunc _ = id