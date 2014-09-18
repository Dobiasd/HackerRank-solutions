import Control.Applicative
import Control.Arrow
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Int
import Data.List.Split
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as Vmu

type Vec = V.Vector Int64

(|>) :: a -> (a -> b) -> b
(|>) x y = y x
infixl 0 |>

main :: IO ()
main = do
    [n, m] <- (splitOn " " >>> map read) <$> getLine
    inputLines <- B.split '\n' <$> B.getContents
    let [a, b, c] = map readInts inputLines
    solve n m a b c |> V.toList >>> map show >>> unwords |> putStrLn

readInts :: B.ByteString -> Vec
readInts = B.split ' ' >>> mapMaybe (B.readInt >>> liftA fst) >>>
    map fromIntegral >>> V.fromList

limit :: Integral a => a -> a
limit = (`mod` 1000000007)

solve :: Int -> Int -> Vec -> Vec -> Vec -> Vec
solve n m a b c = applyChanges changes a
    where
        changes = [(i, \x -> limit $ x * fact ) | (i, fact) <- idxsAndFactors]
        idxsAndFactors = [let ii = fromIntegral i
             in zip [ii-1, ii+ii-1 .. (n-1)] (repeat factor) |
                (i, factor) <- M.assocs factors] |> concat
        factors = buildFactors m b c

-- http://stackoverflow.com/questions/25872149/apply-a-list-of-changes-to-elements-of-a-mutable-vector
applyChanges :: [(Int, Int64 -> Int64)] -> Vec -> Vec
applyChanges changes v = runST $ do
    mV <- V.thaw v
    mapM_ (applyChange mV) changes
    V.freeze mV

applyChange :: (Control.Monad.Primitive.PrimMonad m, Vmu.Unbox t) =>
     Vmu.MVector (Control.Monad.Primitive.PrimState m) t
     -> (Int, t -> t) -> m ()
applyChange mvec (idx, f) = do
    val <- Vmu.read mvec idx
    Vmu.write mvec idx $ f val

buildFactors :: Int -> Vec -> Vec -> M.Map Int64 Int64
buildFactors m b c = M.empty |> comp inserts
        |> M.map fromIntegral >>> M.mapKeys fromIntegral
    where
        inserts = [M.insertWith multAndLimit (b V.! i) (c V.! i) |
            i <- [0 .. m-1]]
            where multAndLimit x y = x * y |> limit

-- http://stackoverflow.com/questions/19777555/most-idiomatic-implementation-of-a-a-a-a
comp :: [b -> b] -> b -> b
comp = foldr (.) id