module Utils where

import qualified Data.Vector as V
import Data.Maybe (mapMaybe)

import Algebra
import DataStructure

-- given the list of edges returns the list of parsed Edges, mainly attaching offset and rank
getParsedEdges :: [Edge] -> [ParsedEdge]
getParsedEdges es = getParsedEdges' es 1 (0, 0) 0 []

getParsedEdges' :: [Edge] -> Int -> Vec -> Double -> [ParsedEdge] -> [ParsedEdge]
getParsedEdges' [] _ _ _ res = reverse res
getParsedEdges' (x:xs) r v l res = getParsedEdges' xs (r + 1) (v &+ geom x) (l + norm ( geom  x))
                (ParsedEdge {edge = x, rank = r, startPoint = v, len = l, parsedObstacles = [], parsedBalconies = [], parsedTents = [], parsedEpafes = []} :res)

-- get Line from edge
getEdgeLine :: ParsedEdge -> Line
getEdgeLine pe = (startPoint pe, geom $ edge pe)

-- returns list of all edge vectors
vectors :: [Edge] -> [Vec]
vectors = map geom

-- given a list of vectors returns the list of points
points :: [Vec] -> [Vec]
points = scanl (&+) (0, 0)

-- returns the point described by OnSkel
pointFromSkel :: [Edge] -> OnSkel -> Vec
pointFromSkel es (a, b) =  sPoint &+ (b &* edgeVector)
  where sPoint = ps V.! (a - 1)
        edgeVector = vs V.! (a - 1)
        ps = V.fromList $ points $ vectors es
        vs = V.fromList $ vectors es

-- returns the total length of the edge before onSkel
getLength :: [ParsedEdge] -> OnSkel -> Double
getLength edgeList (a, b) = len e + (b * norm  (geom $ edge e))
  where e = V.fromList edgeList V.! (a - 1)

