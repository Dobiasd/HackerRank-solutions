f :: Int -> [a] -> [a]
f n = concatMap (replicate n)

-- This part handles the Input and Output and can be used as it is. Do not modify this part.
main :: IO ()
main = do
   n <- readLn :: IO Int
   inputdata <- getContents
   mapM_ print $ f n $ map (read :: String -> Int) $ lines inputdata
   --mapM_ putStrLn $ map show $ f n $ map (read :: String -> Int) $ lines inputdata