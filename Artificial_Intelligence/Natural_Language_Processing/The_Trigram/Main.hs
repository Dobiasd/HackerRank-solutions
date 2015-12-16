import Control.Arrow
import Data.Char
import Data.List
import Data.Ord

main :: IO ()
main = interact solve

solve :: String -> String
solve =
  readTrigrams >>>
  sort >>> group >>> sort >>> reverse >>> sortBy (comparing length) >>>
  reverse >>> head >>> head >>> (++"\n")

readTrigrams :: String -> [String]
readTrigrams =
  filter (/='\r') >>> map (replace '\n' ' ') >>> replace "\n" " " >>>
  wordsWhen isWordBreak >>> triplets >>>
  map (unwords >>> trimNonAlpha >>> map toLower)

replace :: Eq a => a -> a -> a -> a
replace source dest x = if x == source then dest else x

trimNonAlpha :: String -> String
trimNonAlpha = trim $ not <<< isLetter

trim ::(a -> Bool) -> [a] -> [a]
trim p = dropWhile p >>> reverse >>> dropWhile p >>> reverse

triplets :: [a] -> [[a]]
triplets (x:y:z:xs) =  [x, y, z] : triplets (y : z : xs)
triplets _ = []

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
    case dropWhile p s of
        "" -> []
        s' -> w : wordsWhen p s''
            where (w, s'') = break p s'

isWordBreak :: Char -> Bool
isWordBreak ' ' = True
isWordBreak _ = False