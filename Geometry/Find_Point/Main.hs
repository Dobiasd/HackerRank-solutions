import Data.List.Split
import Control.Arrow

data Point = Point Int Int

instance Show Point where
    show (Point x y) = show x ++ " " ++ show y

symPoint :: Point -> Point -> Point
symPoint (Point px py) (Point qx qy) = Point (qx + (qx - px)) (qy + (qy - py))

solve :: String -> String
solve line = show res
    where tokens = splitOn " " line
          [px, py, qx, qy] = map read tokens
          res = symPoint (Point px py) (Point qx qy)

main = do
    _ <- getLine
    interact $ lines >>> map solve >>> unlines