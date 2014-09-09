import Control.Arrow
import Control.Applicative
import qualified Data.Foldable as F
import Data.List
import Data.List.Split

(|>) a b = b a
infixl 0 |>

main :: IO ()
main = do
    [n, m] <- readInts <$> getLine
    ((lines >>> map readBinary >>> solve >>> map show >>> intercalate "\n") <$>
        getContents) >>= putStrLn

readInts :: String -> [Int]
readInts = splitOn " " >>> map read

readBinary :: String -> [Bool]
readBinary = map charToBool

charToBool :: Char -> Bool
charToBool '0' = False
charToBool '1' = True

solve :: [[Bool]] -> [Int]
solve byRow = [bestOverlap, bestCount]
    where
        pairs = byRow |> comb 2 |> map (F.toList >>> toPair)
        overlaps = pairs |> map ratePair
        bestOverlap = maximum overlaps
        bestCount = overlaps |> filter (==bestOverlap) |> length

ratePair :: ([Bool], [Bool]) -> Int
ratePair (a, b) = zipWith (||) a b |> filter id |> length

toPair :: [a] -> (a, a)
toPair [a, b] = (a, b)

-- http://rosettacode.org/wiki/Combinations#Haskell
comb :: Int -> [a] -> [[a]]
comb 0 _      = [[]]
comb m l = [x:ys | x:xs <- tails l, ys <- comb (m-1) xs]