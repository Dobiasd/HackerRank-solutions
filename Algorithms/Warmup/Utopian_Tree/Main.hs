import Control.Arrow

growthCycles :: Int -> Integer
growthCycles n = foldl (flip ($)) 1 $ take n (cycle [(*2), (+1)])

main :: IO ()
main = do
  _ <- getLine
  interact $ lines >>> map (read >>> growthCycles >>> show) >>> unlines