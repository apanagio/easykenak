module Balconies where

import Data.Maybe (mapMaybe)

import Algebra
import DataStructure
import Utils

-- check if balcony passes from startPoint
-- if no mark edges from start to end
-- if yes mark edges from end to end to edgeEnd and from edgeStart to start

-- check if balcony crosses the starting line
-- assume balcony is "normal"
-- check any point on the first edge that should have balcony
-- if it doesn't have balcony, then balcony is not normal

-- gets the points that make the balcony
getBalcPoints :: [Edge] -> Balcony -> [Vec]
getBalcPoints es balc = pointsFrom (pointFromSkel es $ balcStart balc) (balcGeom balc) ++ [pointFromSkel es $ balcEnd balc]

getBalcLines :: [Edge] -> Balcony -> [Line]
getBalcLines es balc = zip (getBalcPoints es balc) (vectorsFromPoints $ getBalcPoints es balc) 

-- moving away perpendicularly from a point onSkel returns list with the balcony edges you meet
getBalcFromSkel :: [Edge] -> Balcony -> OnSkel -> [(Double, Double)]
getBalcFromSkel es balc p = filter ((<1) . snd) $ filter ((>0) . snd) $ filter ((>0) . fst) $ mapMaybe (intersect perp ) (getBalcLines es balc)
  where perp = (pointFromSkel es p, cw $ geom (es !! (fst p - 1)))

-- checks if a point on edges has balcony, returns the distance
hasBalcony :: [Edge] -> Balcony -> OnSkel -> Bool
hasBalcony es balc p = odd $ length $ getBalcFromSkel es balc p 

-- returns the distance of the balcony 
getBalcDist :: [Edge] -> Balcony -> OnSkel -> Maybe Double
getBalcDist es balc p 
  | null l = Nothing
  | otherwise = Just $ fst $ head l
  where l = getBalcFromSkel es balc p


