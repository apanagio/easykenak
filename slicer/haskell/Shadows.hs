module Shadows where

import DataStructure
import Algebra
import Utils

import Data.Function (on)
import Data.List (sortBy, tails)
import Data.Maybe (mapMaybe)

-- project line l1 to line l2
-- only if l2 is "in front" of l1
projectLine :: Line -> Line -> Maybe Interval
projectLine l1 (p, v)
  | not (snd startProj ~< 0) && not (snd endProj ~< 0) = Nothing
  | fst startProj ~= fst endProj = Nothing
  | otherwise = intervalIntersection (0.0, 1.0) (fst startProj, fst endProj)
  where
    startProj =  project p l1
    endProj = project (p &+ v) l1

-- calculates the average distance between lines o, l
-- within the segment (s, e) of line l
getDistance :: Line -> Line -> (OnEdge, OnEdge) -> (Double, Double)
getDistance (p, v) o (s, e) = (norm v * d1, norm v * d2 )
  where d1 = fst $ uintersect (p &+ (s &* v), cw v) o
        d2 = fst $ uintersect (p &+ (e &* v), cw v) o

-- subtracts all following intervals from the current one
-- eg [a, b, c, d] -> [[a - b - c - d], [b - c - d], [c - d], [d]]
mergeIntervalList :: [Interval] -> [[Interval]]
mergeIntervalList iList = zipWith intervalMM iList $ tail $ tails iList

-- returns the part of Line (l) that has shadow from Line (o) and the distance
shadow :: Line -> Line -> Double -> Maybe ParsedObstacle
shadow l o he = do
  inter <- projectLine l o
  return Item {
  fromTo = inter
  , startHeight = (0, 0)
  , endHeight = (0, 0)
  , props = ObstacleProps {
      distance = getDistance l o inter
      , h = he
    }
}

-- ParsedObstacle from Obstacle
obstShadow :: Line -> Obstacle -> Maybe ParsedObstacle
obstShadow l o = shadow l (obstOffset o, obstGeom o) $ obstHeight o

-- ParsedObstacles to specific edge from all Obstacles
getShadowsFromObst :: ParsedEdge -> [Obstacle] -> [ParsedObstacle]
getShadowsFromObst pe = mapMaybe (obstShadow $ getEdgeLine pe)

-- ParsedObstacle from edge to edge
obstFromEdge :: ParsedEdge -> ParsedEdge -> Maybe ParsedObstacle
obstFromEdge e o
  | rank e == rank o = Nothing
  | otherwise = shadow (getEdgeLine e) (getEdgeLine o) (height $ edge o)

-- ParsedObstacles from all edges
getShadowsFromEdge :: ParsedEdge -> [ParsedEdge] -> [ParsedObstacle]
getShadowsFromEdge pe = mapMaybe (obstFromEdge pe)

-- ParsedObstacles from both edges and obstacles
getAllShadows :: ParsedEdge -> Building -> [ParsedObstacle]
getAllShadows pe b = getShadowsFromEdge pe (getParsedEdges $ edges b) ++ getShadowsFromObst pe (obstacles b)

-- merged ParsedObstacles considering which obstacle "hides" others
mergeShadows :: Double -> [ParsedObstacle] -> [ParsedObstacle]
mergeShadows he obst  = concat (zipWith (\o iList -> map (\x -> o { fromTo = x}) iList) sorted merged)
  where
  sorted = sortBy (compare `on` (\o -> (h (props o) - he/2) / uncurry (+) (distance $ props o)) ) obst
  merged = mergeIntervalList $ map fromTo sorted

getMergedShadows :: Building -> ParsedEdge -> [ParsedObstacle]
getMergedShadows b pe = mergeShadows (heightGross b) $ getAllShadows pe b

--add shadows to edge
addShadows :: Building -> ParsedEdge -> ParsedEdge
addShadows b pe = pe {parsedObstacles = getMergedShadows b pe}
