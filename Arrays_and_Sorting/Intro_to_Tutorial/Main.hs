import Control.Applicative
import Control.Arrow
import Data.List
import Data.List.Split
import Data.Maybe

main :: IO ()
main = do
    v <- read <$> getLine
    _ <- getLine
    ar <- readInts <$> getLine
    print $ fromJust $ elemIndex v ar

readInts :: String -> [Int]
readInts = splitOn " " >>> map read