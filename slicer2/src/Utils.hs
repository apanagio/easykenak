module Utils where

import qualified Data.Vector as V
import Data.Maybe (mapMaybe, catMaybes)
import Data.List (sortBy, tails)
import Data.Ord(comparing)
import Control.Arrow ((***))

import Algebra
import DataStructure

type TempObstacle = (Interval, (Double, Double), Double)

-- given a list of vectors and a startPoint returns the list of points
pointsFrom :: Vec -> [Vec] -> [Vec]
pointsFrom = scanl (&+)

-- given a list of vectors returns the list of points
points :: [Vec] -> [Vec]
points = pointsFrom (0, 0)

vectorsFromPoints :: [Vec] -> [Vec]
vectorsFromPoints p = zipWith (&-) (tail p) p

-- returns the point described by OnSkel
pointFromSkel :: [Vec] -> OnSkel -> Vec
pointFromSkel es (a, b) = last (points $ take (a-1) es) &+ (b &* (es !! (a-1)))

-- total length before point
getLength :: [Vec] -> OnSkel -> Double
getLength es (a, b) = foldr ((+).norm) 0 (take (a-1) es) + (b * norm (es !! (a-1)))

-- reverse Item if start > end
reverseItem :: Item a -> Item a
reverseItem i
  | start i < end i = i
  | otherwise = i {start = end i, end = start i, startHeight = endHeight i, endHeight = startHeight i}

-- given a part of edges returns the part of the specific edge that belongs there (assume from < to)
affected :: (OnSkel, OnSkel) -> (OnSkel, OnSkel) -> Maybe (OnSkel, OnSkel)
affected (a1, a2) (b1, b2)
  | fst w > snd w = Nothing
  | otherwise = Just w
  where w = (max a1 b1, min a2 b2)

--enhance (in the future verify) building data
nBuilding :: Building -> Building
nBuilding b = b {
  diafani = map reverseItem $ diafani b
  , adiafani = map reverseItem $ adiafani b
  , levels = map reverseItem $ levels b
  , epafes = map reverseItem $ epafes b
}

-- >>>>>>>>> Balconies <<<<<<<<<<
-- get balcony points
getBalcPoints :: [Vec] -> Balcony -> [Vec]
getBalcPoints es balc = pointsFrom (pointFromSkel es $ balcStart balc) (balcGeom balc) ++ [pointFromSkel es $ balcEnd balc]

getBalcLines :: [Vec] -> Balcony -> [Line]
getBalcLines es balc = zip (getBalcPoints es balc) (vectorsFromPoints $ getBalcPoints es balc)

-- moving away perpendicularly from a point onSkel returns list with the balcony edges you meet
getBalcFromSkel :: [Vec] -> Balcony -> OnSkel -> [(Double, Double)]
getBalcFromSkel es balc (a, b) = filter ((<1) . snd) $ filter ((>0) . snd) $ filter ((>0) . fst) $ mapMaybe (intersect perp ) (getBalcLines es balc)
  where perp = (pointFromSkel es (a, b), cw $ es !! a)

-- checks if a point on edges has balcony (if it crosses the balcony even number of times)
hasBalcony :: [Vec] -> Balcony -> OnSkel -> Bool
hasBalcony es balc p = odd $ length $ getBalcFromSkel es balc p

-- returns the distance of the balcony
getBalcDist :: [Vec] -> Balcony -> OnSkel -> Maybe Double
getBalcDist es balc p
  | null l = Nothing
  | otherwise = Just $ fst $ head l
  where l = getBalcFromSkel es balc p

-- returns the point that if it has balcony then the balcony doesn't cross the start
-- any point on the first edge of the balcony that is after the starting point
getPivotPoint :: Balcony -> OnSkel
getPivotPoint balc
  | fst start == fst end = (fst start, snd start + 0.44 * (snd end - snd start))
  | otherwise = (fst start, snd start + 0.44 * (1 - snd start))
  where start' = min (balcStart balc) (balcEnd balc)
        start = if snd start' == 1 then (fst start' + 1, 0) else start'
        end = max (balcStart balc) (balcEnd balc)

-- returns the parts of the edges that are affected by the balcony
getEdgeWithBalc :: [Vec] -> Balcony -> [(OnSkel, OnSkel)]
getEdgeWithBalc es balc
  | hasBalcony es balc $ getPivotPoint balc = [sorted]
  | otherwise = [((0, 0), fst sorted), (snd sorted, (length es, 1))]
  where sorted = sortTupple (balcStart balc, balcEnd balc)

-- create "normalized" balcony data
nBalcony :: [Vec] -> Balcony -> ParsedBalcony
nBalcony es balc = Item {
  start = balcStart balc
  , end = balcEnd balc
  , startHeight = (0, 0)
  , endHeight = (0, 0)
  , props = BalconyProps {
    parsedBalcLines = getBalcLines es balc
    , parsedBalcHeight = balcHeight balc
    , covers = getEdgeWithBalc es balc
  }
}
-- >>>>>>>>>>>>>> END Balconies <<<<<<<<<<<<<<<<<<<

-- >>>>>>>>>>>>>> Obstacles <<<<<<<<<<<<<<<<<<<<<<<

-- project line l1 to line l2
-- only if l2 is "in front" of l1
-- return part of l1 that gets the projection and
-- distance of the points
projectLine :: Line -> Line -> Maybe (Interval, (Double, Double))
projectLine (p1, v1) (p2, v2)
  | (p1, v1) == (p2, v2) = Nothing
  | not (snd startProj ~< 0) && not (snd endProj ~< 0) = Nothing
  | fst startProj ~= fst endProj = Nothing
  | otherwise = dd <$> i
  where
    startProj =  project p2 (p1, v1)
    endProj = project (p2 &+ v2) (p1, v1)
    i = intervalIntersection (0.0, 1.0) (fst startProj, fst endProj)
    getD s = norm v1 * fst (uintersect (p1 &+ (s &* v1), cw v1) (p2, v2))
    dd i = (i, (getD *** getD) i)

-- subtracts all following intervals from the current one
-- eg [a, b, c, d] -> [[a - b - c - d], [b - c - d], [c - d], [d]]
mergeIntervalList :: [Interval] -> [[Interval]]
mergeIntervalList iList = zipWith intervalMM iList $ tail $ tails iList

getShadowsEdge :: Edges -> Line -> [TempObstacle]
getShadowsEdge es e = catMaybes $ zipWith tt (height es) (map (projectLine e) (zip (points $ geom es) (geom es) ))

getShadowsObst :: [Obstacle] -> Line -> [TempObstacle]
getShadowsObst os e = catMaybes $ zipWith tt (map obstHeight os) (map (projectLine e . getObstLine) os)

appendTupple :: a -> (b, c) -> (b, c, a)
appendTupple a (b, c) = (b, c, a)

tt :: c -> Maybe (a, b) -> Maybe (a, b, c)
tt x = (<$>) (appendTupple x)

getObstLine :: Obstacle -> Line
getObstLine o = (obstOffset o, obstGeom o)

measureObst :: Double -> TempObstacle -> Double
measureObst he (i, (d1, d2), h) = (he - h/2) / (d1 + d2)

getParsedObst :: Int -> TempObstacle -> Interval -> ParsedObstacle
getParsedObst i (_, d, he) (from, to) = Item {
  start = (i, from)
  , end = (i, to)
  , startHeight = (0,0)
  , endHeight = (0,0)
  , props = ObstacleProps {
    distance = d
    , h = he
    }
  }

mergeShadows :: Int -> Double -> [TempObstacle] -> [ParsedObstacle]
mergeShadows i he tes = concat $ zipWith (\o ivals -> map (getParsedObst i o) ivals) sorted merged
  where sorted = sortBy (comparing (measureObst he)) tes
        merged = mergeIntervalList $ map (\(a,b,c) -> a) sorted

getAllShadows :: Building -> [ParsedObstacle]
getAllShadows b = concat $ zipWith f [1..] lines
  where lines = zip (points $ geom eds ) (geom eds)
        eds = edges b
        f  i  e = mergeShadows i (heightGross b) (getShadowsEdge eds e ++ getShadowsObst (obstacles b) e)

-- >>>>>>>>>>>>>> END Obstacles <<<<<<<<<<<<<<<<<<<
