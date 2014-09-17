-- todo: this should be shorter and faster

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Int
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as Vu
import qualified Data.Vector.Unboxed.Mutable as Vmu

(|>) :: a -> (a -> b) -> b
(|>) x y = y x
infixl 0 |>

main :: IO ()
main = do
    [n, m] <- getNumberLine
    [a, b, c] <- replicateM 3 getNumberLine
    solve n m a b c |> map show >>> unwords |> putStrLn

-- http://stackoverflow.com/questions/25891631/how-to-reuse-io-lifted-function-for-different-type
getNumberLine :: Read a => IO [a]
getNumberLine = (splitOn " " >>> map read) <$> getLine

limit :: Integral a => a -> a
limit = (`mod` 1000000007)

solve :: Int -> Int -> [Integer] -> [Integer] -> [Integer] -> [Int64]
solve n m a b c = applyChanges changes (a |> map fromInteger |> Vu.fromList)
        |> Vu.toList
    where
        changes = [(i, \x -> limit $ x * fact ) | (i, fact) <- idxsAndFactors]
        idxsAndFactors = [let ii = fromIntegral i
             in zip [ii-1, ii+ii-1 .. (n-1)] (repeat factor) |
                (i, factor) <- M.assocs factors] |> concat
        factors = buildFactors m b c

-- http://stackoverflow.com/questions/25872149/apply-a-list-of-changes-to-elements-of-a-mutable-vector
applyChanges :: [(Int, Int64 -> Int64)] -> Vu.Vector Int64 -> Vu.Vector Int64
applyChanges changes v = runST $ do
    mV <- Vu.thaw v
    mapM_ (applyChange mV) changes
    Vu.freeze mV

applyChange :: (Control.Monad.Primitive.PrimMonad m, Vmu.Unbox t) =>
     Vmu.MVector (Control.Monad.Primitive.PrimState m) t
     -> (Int, t -> t) -> m ()
applyChange mvec (idx, f) = do
    val <- Vmu.read mvec idx
    Vmu.write mvec idx $ f val

buildFactors :: Int -> [Integer] -> [Integer] -> M.Map Int64 Int64
buildFactors m b c = emptyMap |> comp inserts
        |> M.map fromInteger >>> M.mapKeys fromInteger
    where
        emptyMap = M.empty :: M.Map Integer Integer
        [bVec, cVec] = map V.fromList [b, c]
        inserts = [M.insertWith multAndLimit (bVec V.! i) (cVec V.! i) |
            i <- [0 .. m-1]]
        multAndLimit x y = x * y |> limit

-- http://stackoverflow.com/questions/19777555/most-idiomatic-implementation-of-a-a-a-a
comp :: [b -> b] -> b -> b
comp = foldr (.) id