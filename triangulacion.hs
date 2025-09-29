module Triangulacion(main) where

data Vertex = Vertex Float Float Float deriving (Eq, Show)

data Segment = Segment Int Int deriving (Eq, Show)

weight :: Vertex -> Vertex -> Float
weight (Vertex x0 y0 z0) (Vertex x1 y1 z1) = sqrt $ sq (x0 - x1) + (y0 - y1) + (z0 - z1)
        where sq x = x * x

data Triangle = Triangle Vertex Vertex Vertex deriving (Eq, Show)

perimeter :: Triangle -> Float
perimeter (Triangle v0 v1 v2) = weight v0 v1 + weight v1 v2 + weight v2 v0

newtype Poly = Poly [Vertex] deriving (Eq, Show)

triangulate :: Poly -> [Segment]
triangulate = undefined

main :: IO ()
main = undefined
