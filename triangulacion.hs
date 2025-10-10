{-# OPTIONS_GHC -Wno-x-partial #-}
module Main(main) where

import qualified Data.Array ((!))
import qualified Data.Map.Strict as M
import Control.Monad (replicateM)
import Data.Array hiding ((!))
import Data.Foldable (minimumBy)
import Data.List (sortOn, foldl')
import Data.Map.Strict ((!))
import Data.Ord (comparing)
import System.Process

-- | ¡claro que sí!
(¡) :: Ix i => Array i e -> i -> e
(¡) = (Data.Array.!)

infinity :: Float
infinity = 1.0 / 0.0

data Vertex = Vertex !Float !Float deriving (Eq, Show)

weight :: Vertex -> Vertex -> Float
weight (Vertex x0 y0) (Vertex x1 y1) = sqrt $ sq (x0 - x1) + sq (y0 - y1)
        where sq x = x * x

perimeter :: Vertex -> Vertex -> Vertex -> Float
perimeter v0 v1 v2 = weight v0 v1 + weight v1 v2 + weight v2 v0

type Elements = Array Int Vertex

newtype Poly = Poly Elements deriving (Eq, Show)

regular :: Float -> Int -> Poly
regular r n = Poly . listArray (0, n - 1) $ map (point . angle . fromIntegral) [1..n]
        where point a = Vertex (r * cos a) (r * sin a)
              angle x = x * ((2 * pi) / fromIntegral n)

randomFloat :: Float -> Float -> IO Float
randomFloat a b = read <$> readProcess "python3" ["-c",
        "import random; print(random.uniform(" ++ show a ++ ", " ++ show b ++ "))"] ""

irregular :: Float -> Int -> IO Poly
irregular r p = do
        angles <- replicateM p (randomFloat 0 (2 * pi))
        let points = map (\a -> Vertex (r * cos a) (r * sin a)) angles
        pure . Poly . listArray (0, p - 1) $
                sortOn (\(Vertex x y) -> atan2 y x) points

type Cache = M.Map (Int, Int) Float

cost :: Cache -> Elements -> Int -> Int -> (Cache, Float)
cost !cache vs !i !j
        | j < i = cost cache vs j i
        | j - i <= 2 = (cache, 0.0)
        | M.member (i, j) cache = (cache, cache ! (i, j))
        | otherwise = foldl' it (cache, infinity) options
       where calc cache' k =
                let (!cache'', costLeft) = cost cache' vs i k
                    (cache''', costRight) = cost cache'' vs k j
                    w = weight (vs ¡ i) (vs ¡ k) + costLeft + costRight
                 in (M.insert (i, j) w cache''', w)
             it (!cache', minW) k =
                let (cache'', w) = calc cache' k
                 in (cache'', min w minW)
             options = [i + 1..j - 1]

triangulate :: Poly -> Float
triangulate (Poly !vs) = snd $ cost M.empty vs i j
        where (!i, !j) = bounds vs

writePoly :: Poly -> IO ()
writePoly (Poly vs) = do
        let v (Vertex x y) = "v " ++ show x ++ ' ' : show y
            l i j = "l " ++ show i ++ ' ' : show j
            idxs = [0..snd (bounds vs)]
            pairs = zipWith (\a b -> (a + 1, b + 1)) idxs (tail idxs ++ [head idxs])
            content = unlines (map (\i -> v (vs ¡ i)) idxs)
                   ++ unlines (map (uncurry l) pairs)
        writeFile "triangulacion.obj" content

main :: IO ()
main = irregular 1 200 >>= print . triangulate
