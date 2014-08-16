growthCycles :: Int -> Integer
growthCycles n = foldl (flip ($)) 1 $ take n (cycle [(*2), (+1)])

main = do
  _ <- getLine
  interact $ unlines . map (show . growthCycles . read) . lines