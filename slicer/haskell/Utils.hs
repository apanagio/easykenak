module Utils where

import qualified Data.Vector as V

import Algebra
import DataStructure

data ParsedObstacle = ParsedObstacle {
  start :: OnEdge
  , end :: OnEdge
  , distance :: Double
  , h :: Double
} deriving Show

-- calculates the average distance between lines o, l 
-- within the segment (s, e) of line l
getDistance :: Line -> Line -> (OnEdge, OnEdge) -> Double 
getDistance (p, v) o (s, e) = (norm v) * ( d1 + d2 ) / 2
  where d1 = fst $ uintersect (p &+ (s &* v), ccw v) o
        d2 = fst $ uintersect (p &+ (e &* v), ccw v) o

-- returns the part of Line (l) that has shadow from Line (o) and the distance
shadow :: Line -> Line -> Double -> Maybe ParsedObstacle
shadow l o h = do
  inter <- projectLine l o
  return ParsedObstacle {
  start = fst inter
  , end = snd inter
  , distance = getDistance l o inter
  , h = h
}

obstShadow :: Line -> Obstacle -> Maybe ParsedObstacle
obstShadow = undefined

-- returns list of all edge vectors
vectors :: [Edge] -> [Vec]
vectors = map geom

-- given a list of vectors returns the list of points 
points :: [Vec] -> [Vec]
points = scanl (&+) (0, 0)

-- returns the point described by OnSkel
pointFromSkel :: [Edge] -> OnSkel -> Vec
pointFromSkel es (a, b) =  startPoint &+ (b &* edgeVector)
  where startPoint = ps V.! (a - 1)
        edgeVector = vs V.! (a - 1)
        ps = V.fromList $ points $ vectors es
        vs = V.fromList $ vectors es

-- reverse epafi if start > end
reverseEpafi :: Epafi -> Epafi
reverseEpafi ep 
  | fst (epfStart ep) <= fst (epfEnd ep) = ep
  | otherwise = ep {epfStart = epfEnd ep, epfEnd = epfStart ep}
  
  
