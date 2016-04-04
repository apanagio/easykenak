module Algebra
(Vec
) where

type Vec = (Double, Double)
type Line = (Vec, Vec)

eps = 1e-10

(~=) :: Double -> Double -> Bool
(~=) a b = a - b < eps && a - b > -eps 

(~>) :: Double -> Double -> Bool
(~>) a b = a - b > -eps 

(~<) :: Double -> Double -> Bool
(~<) a b = a - b < eps 

(&+) :: Vec -> Vec -> Vec
(&+) (a1, a2) (b1, b2) = (a1+b1, a2+b2) 

(&.) :: Vec -> Vec -> Double
(&.) (a1, a2) (b1, b2) = a1*b1 + a2*b2 

norm :: Vec -> Double
norm v = sqrt $ v &. v 

cross :: Vec -> Vec -> Double
cross (a1, a2) (b1, b2) = a1*b2 - a2*b1 

(&*) :: Double -> Vec -> Vec
(&*) a (b1, b2) = (a * b1, a * b2)

(&-) :: Vec -> Vec -> Vec
(&-) (a1, a2) (b1, b2) = (a1 - b1, a2 - b2) 

intersect :: Line -> Line -> Maybe (Double, Double)
intersect (v1, v2) (w1, w2) 
  | c ~= 0 = Nothing
  | otherwise = Just (t, u)
    where t = (cross d w2) / c
          u = (cross d v2) / c
          d = v1 &- w1
          c = cross v2 w2
