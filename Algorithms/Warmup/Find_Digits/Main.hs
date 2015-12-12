import Control.Arrow

(|>) :: a -> (a -> b) -> b
(|>) x y = y x
infixl 0 |>

main :: IO ()
main = do
    _ <- getLine
    interact $ lines >>> map (solve >>> show) >>> unlines

solve :: String -> Int
solve s = filter (`divides` n) digits |> length
    where digits = map ((:[]) >>> read) s
          n      = read s

divides :: Integer -> Integer -> Bool
divides 0 _ = False
divides a b = b `mod` a == 0