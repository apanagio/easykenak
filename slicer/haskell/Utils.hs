module Utils where

import qualified Data.Vector as V
import Data.Maybe (catMaybes)

import Algebra
import DataStructure

data ObstacleProps = ObstacleProps {
  distance :: Double
  , h :: Double
}  deriving Show

type EpafiProps = Int

type ParsedObstacle = Item ObstacleProps

data ParsedEdge = ParsedEdge {
  edge :: Edge
  , rank :: Int
  , startPoint :: Vec
  , parsedObstacles :: [ParsedObstacle]
  , parsedBalconies :: [ParsedObstacle]
  , parsedTents :: [ParsedObstacle]
  , parsedEpafes :: [Item EpafiProps]
} deriving Show

-- given the list of edges returns the list of parsed Edges, mainly attaching offset and rank
getParsedEdges :: [Edge] -> [ParsedEdge]
getParsedEdges es = getParsedEdges' es 1 (0, 0) []

getParsedEdges' [] _ _ res = reverse res
getParsedEdges' (x:xs) r v res = getParsedEdges' xs (r + 1) (v &+ geom x) ((ParsedEdge {edge = x, rank = r, startPoint = v, parsedObstacles = [], parsedBalconies = [], parsedTents = [], parsedEpafes = []}) :res)

-- get Line from edge
getEdgeLine :: ParsedEdge -> Line
getEdgeLine pe = (startPoint pe, geom $ edge pe)

-- calculates the average distance between lines o, l 
-- within the segment (s, e) of line l
getDistance :: Line -> Line -> (OnEdge, OnEdge) -> Double 
getDistance (p, v) o (s, e) = (norm v) * ( d1 + d2 ) / 2
  where d1 = fst $ uintersect (p &+ (s &* v), cw v) o
        d2 = fst $ uintersect (p &+ (e &* v), cw v) o

-- returns the part of Line (l) that has shadow from Line (o) and the distance
shadow :: Line -> Line -> Double -> Maybe ParsedObstacle
shadow l o h = do
  inter <- projectLine l o
  return Item {
  start = fst inter
  , end = snd inter
  , startHeight = (0, 0)
  , endHeight = (0, 0)
  , props = ObstacleProps {
      distance = getDistance l o inter
      , h = h
    }
}

obstShadow :: Line -> Obstacle -> Maybe ParsedObstacle
obstShadow l o = shadow l (obstOffset o, obstGeom o) $ obstHeight o

getShadowsFromObst :: ParsedEdge -> [Obstacle] -> [ParsedObstacle]
getShadowsFromObst pe os = catMaybes $ map (obstShadow $ getEdgeLine pe) os

obstFromEdge :: ParsedEdge -> ParsedEdge -> Maybe ParsedObstacle
obstFromEdge e o 
  | rank e == rank o = Nothing
  | otherwise = shadow (getEdgeLine e) (getEdgeLine o) (height $ edge o)

getShadowsFromEdge :: ParsedEdge -> [ParsedEdge] -> [ParsedObstacle]
getShadowsFromEdge pe pes = catMaybes $ map (obstFromEdge pe) pes

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
reverseEpafi ep = ep {epfStart = min (epfEnd ep) (epfStart ep), epfEnd = max (epfEnd ep) (epfStart ep)}
  
  
