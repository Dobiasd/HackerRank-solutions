import Data.List.Split
import Control.Arrow

(|>) x y = y x
infixl 0 |>

data Point = Point Int Int Int deriving Show

dotProduct :: Point -> Point -> Int
dotProduct (Point x1 y1 z1) (Point x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

crossProduct :: Point -> Point -> Point
crossProduct (Point ax ay az) (Point bx by bz) = Point (ay*bz - az*by)
                                                       (az*bx - ax*bz)
                                                       (ax*by - ay*bx)

tripleProduct :: Point -> Point -> Point -> Int
tripleProduct p1 p2 p3 = p1 `dotProduct` (p2 `crossProduct` p3)

subVec :: Point -> Point -> Point
subVec (Point ax ay az) (Point bx by bz) = Point (bx-ax) (by-ay) (bz-az)

pointsInPlane :: [Point] -> Bool
pointsInPlane points = tripleProduct d1 d2 d3 == 0
    where
        [p1, p2, p3, p4] = points
        d1 = p2 `subVec` p1
        d2 = p3 `subVec` p1
        d3 = p4 `subVec` p1

strToPoint :: String -> Point
strToPoint s = Point x y z
    where [x, y, z] = splitOn " " s |> map read

boolToString :: Bool -> String
boolToString True = "YES"
boolToString False = "NO"

solve :: [String] -> String
solve = map strToPoint >>> pointsInPlane >>> boolToString

main = do
    _ <- getLine
    interact $ lines >>> chunksOf 4 >>> map solve >>> unlines