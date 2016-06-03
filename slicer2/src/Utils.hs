module Utils where

import qualified Data.Vector as V
import Data.Maybe (mapMaybe)

import Algebra
import DataStructure

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
affected :: OnSkel -> OnSkel -> Int -> Maybe Interval
affected from to which
  | which < fst from = Nothing
  | which > fst to = Nothing
  | otherwise = Just (a, b)
  where a = if which > fst from then 0 else snd to
        b = if which < fst to then 1 else snd to
   
nBuilding :: Building -> Building
nBuilding b = b {
  diafani = map reverseItem $ diafani b
  , adiafani = map reverseItem $ adiafani b
  , levels = map reverseItem $ levels b
  , epafes = map reverseItem $ epafes b
}

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
