import Data.List

(|>) x y = y x
infixl 0 |>

solve :: String -> String
solve str = vals |> sort |> countUnique
              |> filter (\(_,l) -> l < 2) |> fst . head |> show
  where vals :: [Integer]
        vals = (map read . words) str
        countUnique = map (\xs@(x:_) -> (x, length xs)) . group . sort

main = do
    _ <- getLine
    val2 <- getLine
    putStrLn $ solve val2