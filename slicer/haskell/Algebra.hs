module Algebra where

type Vec = (Double, Double)

-- ~ line described from point and direction vector
type Line = (Vec, Vec)

type Interval = (Double, Double)

-- q a b = (a, b)
eps :: Double
eps = 1e-10

(~=) :: Double -> Double -> Bool
(~=) a b = a - b < eps && a - b > -eps

(~>) :: Double -> Double -> Bool
(~>) a b = a - b > -eps

(~<) :: Double -> Double -> Bool
(~<) a b = a - b < eps


elementMult :: Vec -> Vec -> Vec
elementMult (a1, a2) (b1, b2) = (a1 * b1, a2 * b2)
(|*) = elementMult

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

-- ~ clockwise rotation 90deg
cw :: Vec -> Vec
cw (a, b) = (b, -a)

-- ~ counterclockwise rotation -90deg
ccw :: Vec -> Vec
ccw (a, b) = (-b, a)

dist :: Vec -> Vec -> Double
dist v1 v2 = norm (v2 &- v1)

-- Are 2 vectors parallel?
par ::  Vec -> Vec -> Bool
par v w = cross v w ~= 0

-- line given in the form of (point, vector)
-- if there is intersection a tupple (a, b) is returned
-- a: distance from line1 point relevant to the size of line1 vector
-- b: equivalent for line2
intersect :: Line -> Line -> Maybe (Double, Double)
intersect (v1, v2) (w1, w2)
  | par v2 w2 = Nothing
  | otherwise = Just $ uintersect (v1, v2) (w1, w2)

--unsafe intersect, no check, can throw error if lines parallel
uintersect :: Line -> Line -> (Double, Double)
uintersect (v1, v2) (w1, w2) = (t, u) where
  t = cross w2 d / c
  u = cross v2 d / c
  d = v1 &- w1
  c = cross v2 w2

-- sort a tupple so that snd >= fst
sortTupple :: (Double, Double) -> (Double, Double)
sortTupple (a, b)
  | b < a = (b, a)
  | otherwise = (a, b)

width :: Interval -> Double
width (a, b) = b - a

-- interval intersection
intervalIntersection :: Interval -> Interval -> Maybe Interval
intervalIntersection a b = interval' (sortTupple a) (sortTupple b)

interval' (a1, a2) (b1, b2)
  | a2 <= b1 = Nothing
  | a1 >= b2 = Nothing
  | otherwise = Just (max a1 b1, min a2 b2)

-- interval subtraction remove from big interval the common parts
intervalSub :: Interval -> Interval -> [Interval]
intervalSub small big= case i of
 Nothing -> [big]
 Just (x, y) -> filter ((>0) . width) [(a, x), (y, b)]
 where
   i = intervalIntersection small big
   a = fst $ sortTupple big
   b = snd $ sortTupple big

-- remove i from all intervals in the ilist
intervalMultiSub :: [Interval] -> Interval -> [Interval]
intervalMultiSub iList i = concatMap (intervalSub i) iList

-- remove all intervals in iList1 from intervals in iList2
intervalMM :: Interval -> [Interval] -> [Interval]
intervalMM i1 iList2 = foldl intervalMultiSub [i1] iList2

-- projects point p to line (w1, w2)
project :: Vec -> Line -> (Double, Double)
project p (w1, w2) = uintersect (w1, w2) (p, cw w2)
