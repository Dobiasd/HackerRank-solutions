import Data.List
import Data.List.Split
import Control.Arrow

anagramCount :: String -> Int
anagramCount s = if length s `mod` 2 /= 0 then -1 else length $ a \\ b
  where [a, b] = chunksOf (length s `div` 2) s

main = do
  _ <- getLine
  interact $ lines >>> map (show . anagramCount) >>> unlines