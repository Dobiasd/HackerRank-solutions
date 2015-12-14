import Control.Arrow
main = interact $ lines >>> tail >>> map (read >>> (+1) >>> show) >>> unlines