import Data.List

(|>) x y = y x
infixl 0 |>

intersectLists :: Eq a => [[a]] -> [a]
intersectLists = foldl1 intersect

countGems :: String -> Int
countGems s = lines s |> map sort |> map nub |> intersectLists |> length

main = do
  _ <- getLine
  interact $ show . countGems