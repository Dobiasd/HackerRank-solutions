import Data.List
import Control.Arrow

main :: IO ()
main = getLine >>= (putStrLn . boolAsWord . solve)

boolAsWord :: Bool -> String
boolAsWord True = "YES"
boolAsWord False = "NO"

solve :: String -> Bool
solve = sort >>> group >>> map length >>> filter odd >>> length >>> (<2)