import Data.List
import Control.Arrow

intersectLists :: Eq a => [[a]] -> [a]
intersectLists = foldl1 intersect

countGems :: String -> Int
countGems = lines s >>> map sort >>> map nub >>> intersectLists >>> length

main = do
  _ <- getLine
  interact $ countGems >>> show