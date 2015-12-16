import Control.Arrow
import Data.List

main :: IO ()
main = interact solve

type Value = Float
data RGB = RGB Value Value Value
data Gray = Gray Value
type ImageRow a = [a]
data Image2d pixelType = Image2d [ImageRow pixelType]
data Position = Position Int Int

(|>) :: x -> (x -> y) -> y
(|>) x y = y x
infixl 0 |>

instance Show RGB where
    show (RGB r g b) = "RGB " ++ show r ++ "/" ++ show g ++ "/" ++ show b

instance Show Gray where
    show (Gray v) = "Gray " ++ show v

class PixelType pixelType where
    divide :: pixelType -> Float -> pixelType
    add :: pixelType -> pixelType -> pixelType
    mult :: pixelType -> Float -> pixelType
    black :: pixelType
    white :: pixelType
    sumUp :: [pixelType] -> pixelType
    sumUp = foldr add white
    listToColor :: [Value] -> pixelType
    readColor :: String -> pixelType
    readColor = wordsWhen (==',') >>> map read >>> listToColor

instance PixelType RGB where
    divide (RGB r g b) x = RGB (r/x) (g/x) (b/x)
    add (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (r1+r2) (g1+g2) (b1+b2)
    mult (RGB r1 g1 b1) f = RGB (f*r1) (f*g1) (f*b1)
    black = RGB 0 0 0
    white = RGB 255 255 255
    listToColor [r, g, b] = RGB r g b
    listToColor _ = error "invalid RGB list"

instance PixelType Gray where
    divide (Gray v) x = Gray (v/x)
    add (Gray v1) (Gray v2) = Gray (v1 + v2)
    mult (Gray v) f = Gray (f*v)
    black = Gray 0
    white = Gray 255
    listToColor [v] = Gray v
    listToColor _ = error "invalid Gray list"

class PixelType pixelType => Image pixelType where
    pixelSum :: Image2d pixelType -> pixelType
    pixelSum (Image2d rows) = rows |> map sumUp |> sumUp

    averageColor :: Image2d pixelType -> pixelType
    averageColor image = divide sumVal divisor
        where
            sumVal = pixelSum image
            divisor = pixelCount image |> fromIntegral
    imageRows :: Int -> Int -> Image2d pixelType -> Image2d pixelType
    imageRows y0 y1 (Image2d rows) = rows |> slice y0 y1 |> Image2d

    imageSize :: Image2d pixelType -> Position
    imageSize (Image2d rows) = Position (length (head rows)) (length rows)

    getRows :: Image2d pixelType -> [ImageRow pixelType]
    getRows (Image2d rows) = rows

    regionOfInterest :: Position -> Position -> Image2d pixelType -> Image2d pixelType
    regionOfInterest (Position x0 y0) (Position x1 y1) =
        imageRows y0 y1
        >>> transposeImage
        >>> imageRows x0 x1
        >>> transposeImage

    transposeImage :: Image2d pixelType -> Image2d pixelType
    transposeImage = getRows >>> transpose >>> Image2d

    pixelCount :: Image2d pixelType -> Int
    pixelCount = getRows >>> map length >>> sum

    readImage :: String -> Image2d pixelType
    readImage = lines >>> map (words >>> map readColor) >>> Image2d

instance Image RGB where

instance Image Gray where

toValue :: Gray -> Value
toValue (Gray v) = v

rgbToGray :: RGB -> Gray
rgbToGray (RGB r g b) = Gray (0.299 * r + 0.587 * g + 0.114 * b)

grayToRgb :: Gray -> RGB
grayToRgb (Gray v) = RGB v v v

readImageRGB :: String -> Image2d RGB
readImageRGB = readImage

solve :: String -> String
solve =
    readImage
    >>> imageRows 0 2
    >>> averageColor
    >>> rgbToGray
    >>> toValue
    >>> toDayTime
    where
        toDayTime val = if val >= 96 then "day" else "night"

slice :: Int -> Int -> [a] -> [a]
slice begin end xs = take (end - begin) (drop begin xs)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
    case dropWhile p s of
        "" -> []
        s' -> w : wordsWhen p s''
            where (w, s'') = break p s'