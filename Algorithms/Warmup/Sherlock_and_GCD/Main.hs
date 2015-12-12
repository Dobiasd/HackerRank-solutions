import Control.Arrow
import Data.List
import Data.List.Split

(|>) :: a -> (a -> b) -> b
(|>) x y = y x
infixl 0 |>

main :: IO ()
main = do
    _ <- getLine
    interact $ lines >>> chunksOf 2 >>> map (solve >>> showBool)
        >>> intercalate "\n" >>> (++ "\n")

solve :: [String] -> Bool
solve [_, line] = readInts line |> gcdmany |> (==1)
solve _ = error "list must have length 2"

showBool :: Bool -> String
showBool True = "YES"
showBool False = "NO"

readInts :: String -> [Int]
readInts = splitOn " " >>> map read

gcdmany :: Integral a => [a] -> a
gcdmany = foldl gcd 0