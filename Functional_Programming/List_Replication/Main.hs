f :: Int -> [a] -> [a]
f n = concatMap (replicate n)

main :: IO ()
main = do
   n <- readLn :: IO Int
   inputdata <- getContents
   mapM_ print $ f n $ map (read :: String -> Int) $ lines inputdata