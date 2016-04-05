module Algebra
(Vec(..)
) where

type Vec = (Double, Double)

-- ~ first vector is a point in line, second vector is the direction vector
-- ~ segments can be defined as lines with a direction vector same size as segment 
type Line = (Vec, Vec)

eps = 1e-10

(~=) :: Double -> Double -> Bool
(~=) a b = a - b < eps && a - b > -eps 

(~>) :: Double -> Double -> Bool
(~>) a b = a - b > -eps 

(~<) :: Double -> Double -> Bool
(~<) a b = a - b < eps 

addVector :: Vec -> Vec -> Vec
addVector (a1, a2) (b1, b2) = (a1 + b1, a2 + b2) 
(&+) = addVector

subVector :: Vec -> Vec -> Vec
subVector (a1, a2) (b1, b2) = (a1 - b1, a2 - b2) 
(&-) = subVector

dotProduct :: Vec -> Vec -> Double
dotProduct (a1, a2) (b1, b2) = a1 * b1 + a2 * b2 
(&.) = dotProduct

norm :: Vec -> Double
norm v = sqrt $ v &. v 

cross :: Vec -> Vec -> Double
cross (a1, a2) (b1, b2) = a1 * b2 - a2 * b1 
(&#) = cross

scalar :: Double -> Vec -> Vec
scalar a (b1, b2) = (a * b1, a * b2)
(&*) = scalar

-- ~ clockwise rotation
cw :: Vec -> Vec
cw (a, b) = (b, -a)

-- ~ counterclockwise rotation
ccw :: Vec -> Vec
ccw (a, b) = (-b, a)

dist :: Vec -> Vec -> Double
dist v1 v2 = norm (v2 &- v1)

par ::  Vec -> Vec -> Bool
par v w = cross v w ~= 0

intersect :: Line -> Line -> Maybe (Double, Double)
intersect (v1, v2) (w1, w2)
  | par v2 w2 = Nothing
  | otherwise = Just $ uintersect (v1, v2) (w1, w2)

uintersect :: Line -> Line -> (Double, Double)
uintersect (v1, v2) (w1, w2) = (t, u) where
  t = (cross d w2) / c
  u = (cross d v2) / c
  d = v1 &- w1
  c = cross v2 w2

-- ~ project :: Vec -> Line (Double, Double)
