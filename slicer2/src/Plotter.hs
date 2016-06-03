module Plotter where

import DataStructure
import Algebra
import Utils

plot :: Building -> String
plot b = plotVectors (points $ geom $ edges b)
  ++ "\n\n" ++ plotVectorList (uGetItems (geom $ edges b) (diafani b))
  ++ "\n\n" ++ plotVectorList (uGetItems (geom $ edges b) (adiafani b))
  ++ "\n\n" ++ plotVectorList (uGetItems (geom $ edges b) (levels b))
  ++ "\n\n" ++ plotVectorList (uGetItems (geom $ edges b) (epafes b))
  ++ "\n\n" ++ plotVectorList (getBalconies (geom $ edges b) (balconies b))
  ++ "\n\n" ++ plotVectorList (getBalconies (geom $ edges b) (tents b))
  ++ "\n\n" ++ plotVectorList (getObstacles $ obstacles b)

plotPoint :: Vec -> String
plotPoint (a, b) = show a ++ " " ++ show b ++ "\n"

plotVectors :: [Vec] -> String
plotVectors vs = foldr1 (++) (map plotPoint vs) ++ "\n"

plotVectorList :: [[Vec]] -> String
plotVectorList [] = ""
plotVectorList vs = foldr1 (++) (map plotVectors vs)

-- all the points between two points (assume sorted)
pointsBetween :: OnSkel -> OnSkel -> [OnSkel]
pointsBetween (s1, s2) (e1, e2) = (s1, s2) : zip [s1+1..e1] [0, 0..] ++ [(e1, e2)]

-- plot Items (diafani, adiafani, levels, epafes)
uGetItems :: [Vec] -> [Item a] -> [[Vec]]
uGetItems es = map getItem
  where getItem  i = map (pointFromSkel es) (pointsBetween (start i) (end i))  

getBalconies :: [Vec] -> [Balcony] -> [[Vec]]
getBalconies es = map (getBalcPoints es)

-- plot Obstacles
getObstacles :: [Obstacle] -> [[Vec]]
getObstacles = map $ \o -> pointsFrom (obstOffset o) [obstGeom o]
