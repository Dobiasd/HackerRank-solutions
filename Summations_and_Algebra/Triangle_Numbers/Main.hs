import Data.List
import Data.Maybe

(|>) x y = y x
infixl 0 |>

main = do
    _ <- getLine
    interact $ unlines .
               map (show . posOfFirstEvenNumInTriangleRowN . read) .
               lines


posOfFirstEvenNumInTriangleRowN :: Int -> Int
posOfFirstEvenNumInTriangleRowN n = triangle !! (n - 1) |> firstPos even |> (+1)


triangle :: [[Integer]]
triangle = iterate nextRow [1]

nextRow :: [Integer] -> [Integer]
nextRow row = zipWith3 (\a b c -> a+b+c)
                       (0 : init padded) padded (tail padded ++ [0])
    where padded = 0 : row ++ [0]


firstPos :: (a -> Bool) -> [a] -> Int
firstPos pred xs = fromMaybe (-1) $ findIndex pred xs