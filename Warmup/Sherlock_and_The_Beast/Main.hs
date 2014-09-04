import Control.Arrow

main :: IO ()
main = do
    _ <- getLine
    interact $ lines >>> map (read >>> solve) >>> unlines

-- x fives ++ y threes
solve :: Int -> String
solve n = case biggestPossibleX 3 5 n of
    Just (x, y) -> replicate (3*x) '5' ++ replicate (5*y) '3'
    Nothing -> "-1"

-- n = a * x + b * y
-- find biggest possible x with smallest y
biggestPossibleX :: Int -> Int -> Int -> Maybe (Int, Int)
biggestPossibleX a b n = maybeHead solutions
    where solutions = [(x, (n - a * x) `div` b) |
                        x <- cd,
                        (n - a * x) `mod` b == 0 ]
          cd = [upper, upper-1 .. 0]
          upper = n `div` a

maybeHead :: [a] -> Maybe a
maybeHead []     = Nothing
maybeHead (x:xs) = Just x