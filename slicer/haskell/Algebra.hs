module Algebra where

type Vec = (Double, Double)

-- ~ line described from point and direction vector
type Line = (Vec, Vec)

q a b = (a, b)

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
  t = (cross w2 d) / c
  u = (cross v2 d) / c
  d = v1 &- w1
  c = cross v2 w2

project :: Vec -> Line -> (Double, Double)
project v (w1, w2) = uintersect (w1, w2) (v, cw w2)
