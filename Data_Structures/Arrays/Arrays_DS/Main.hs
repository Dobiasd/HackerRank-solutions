import Control.Arrow

main :: IO ()
main = interact $ lines >>> tail >>> head >>> words >>> reverse >>> unwords