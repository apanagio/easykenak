module Utils where

import qualified Data.Vector as V

import Algebra
import DataStructure

-- given the list of edges returns the list of parsed Edges, mainly attaching offset and rank
getParsedEdges :: [Edge] -> [ParsedEdge]
getParsedEdges es = getParsedEdges' es 1 (0, 0) []

getParsedEdges' [] _ _ res = reverse res
getParsedEdges' (x:xs) r v res = getParsedEdges' xs (r + 1) (v &+ geom x) ((ParsedEdge {edge = x, rank = r, startPoint = v, parsedObstacles = [], parsedBalconies = [], parsedTents = [], parsedEpafes = []}) :res)

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
pointFromSkel es (a, b) =  startPoint &+ (b &* edgeVector)
  where startPoint = ps V.! (a - 1)
        edgeVector = vs V.! (a - 1)
        ps = V.fromList $ points $ vectors es
        vs = V.fromList $ vectors es

-- reverse epafi if start > end
reverseEpafi :: Epafi -> Epafi
reverseEpafi ep = ep {epfStart = min (epfEnd ep) (epfStart ep), epfEnd = max (epfEnd ep) (epfStart ep)}
  
  
