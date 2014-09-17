main :: IO ()
main = fmap solve (sequence [readLn, readLn]) >>= print

solve :: [Integer] -> Integer
solve = sum