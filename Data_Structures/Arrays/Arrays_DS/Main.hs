import Control.Arrow
main = interact $ lines >>> tail >>> head >>> words >>> reverse >>> unwords