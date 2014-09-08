import Control.Arrow
import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe

(|>) a b = b a
infixl 0 |>

main :: IO ()
main = do
    _ <- getLine
    interact $ lines >>> chunksOf 2 >>> map (solve >>> showBool)
        >>> intercalate "\n" >>> (++ "\n")

solve :: [String] -> Bool
solve [_, line] = readInts line |> gcdmany |> (==1)

showBool :: Bool -> String
showBool True = "YES"
showBool False = "NO"

readInts :: String -> [Int]
readInts = splitOn " " >>> map read

gcdmany :: Integral a => [a] -> a
gcdmany = foldl gcd 0