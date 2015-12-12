import Data.List
import Control.Arrow

intersectLists :: Eq a => [[a]] -> [a]
intersectLists = foldl1 intersect

countGems :: String -> Int
countGems = lines >>> map sort >>> map nub >>> intersectLists >>> length

main :: IO ()
main = do
  _ <- getLine
  interact $ countGems >>> show