module Shadows where

import DataStructure
import Algebra
import Utils

import Data.Function (on)
import Data.List (sortBy, tails)
import Data.Maybe (catMaybes)

-- project line l1 to line l2
-- only if l2 is "in front" of l1
projectLine :: Line -> Line -> Maybe Interval
projectLine l1 (p, v)
  | not (snd startProj ~< 0) && not (snd endProj ~< 0) = Nothing
  | fst startProj ~= fst endProj = Nothing
  | otherwise = intervalIntersection (0.0, 1.0) (fst $ startProj, fst $ endProj)
  where 
    startProj =  project p l1
    endProj = project (p &+ v) l1
    
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
  fromTo = inter
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

getAllShadows :: ParsedEdge -> Building -> [ParsedObstacle]
getAllShadows pe b = (getShadowsFromEdge pe $ getParsedEdges $ edges b) ++ (getShadowsFromObst pe $ obstacles b)

mergeShadows :: Double -> [ParsedObstacle] -> [ParsedObstacle]
mergeShadows height obst  = map (\x -> (head obst) { fromTo = x} ) merged
  where 
  sorted = sortBy (compare `on` (\o -> ((h $ props o) - height/2) / (distance $ props o)) ) obst 
  merged = mergeIntervalList $ map fromTo sorted

-- subtracts all following intervals from the current one 
mergeIntervalList :: [Interval] -> [Interval]
mergeIntervalList iList = concat $ zipWith intervalMM iList $ tail $ tails iList
