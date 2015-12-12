import Control.Arrow
import Data.List

main :: IO ()
main = do
    _ <- getLine
    interact $ lines >>> map (read >>> solve >>> showRes) >>> intercalate "\n"

solve :: Integer -> Bool
solve n = elem n $ takeWhile (<=n) fibs

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

showRes :: Bool -> String
showRes True = "IsFibo"
showRes False = "IsNotFibo"