import Data.List.Split
import Data.Char
import Control.Arrow

palindromeMoveCnt :: String -> Int
palindromeMoveCnt s = sum diffs
  where [start, restM1] = (chunksOf $ (length s + 1) `div` 2) s
        rest = if length s `mod` 2 /= 0 then last start : restM1 else restM1
        f a b = abs $ ord b - ord a
        diffs = zipWith f start (reverse rest)

main :: IO ()
main = do
  _ <- getLine
  interact $ lines >>> map (palindromeMoveCnt >>> show) >>> unlines