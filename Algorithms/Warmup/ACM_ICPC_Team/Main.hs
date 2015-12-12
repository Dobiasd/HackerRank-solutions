import Control.Arrow
import Control.Applicative
import qualified Data.Foldable as F
import Data.List
import Data.List.Split

(|>) :: a -> (a -> b) -> b
(|>) x y = y x
infixl 0 |>

main :: IO ()
main = do
    _ <- getLine
    ((lines >>> map readBinary >>> solve >>> map show >>> intercalate "\n") <$>
        getContents) >>= putStrLn

readInts :: String -> [Int]
readInts = splitOn " " >>> map read

readBinary :: String -> [Bool]
readBinary = map charToBool

charToBool :: Char -> Bool
charToBool '1' = True
charToBool _ = False

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
toPair _ = error "list must have length 2"

-- http://rosettacode.org/wiki/Combinations#Haskell
comb :: Int -> [a] -> [[a]]
comb 0 _      = [[]]
comb m l = [x:ys | x:xs <- tails l, ys <- comb (m-1) xs]