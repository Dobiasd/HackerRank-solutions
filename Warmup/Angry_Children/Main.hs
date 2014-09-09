import Control.Applicative
import Data.List

main :: IO ()
main = do
    _ <- getLine
    k <- read <$> getLine
    xs <- (map read . lines) <$> getContents
    print $ solve k xs

solve :: Int -> [Int] -> Int
solve k xs = minimum $ map (uncurry (-)) pairs
    where sorted = sort xs
          back = drop (k-1) sorted
          pairs = zip back sorted