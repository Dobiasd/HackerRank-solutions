import Data.Bits

maxXor :: Int -> Int -> Int
maxXor l r = maximum [xor a b | a <- range, b <- range]
  where range = [l..r]

main :: IO ()
main = do
  l <- readLn
  r <- readLn
  print $ maxXor l r