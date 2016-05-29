module Plotter where

import DataStructure
import Algebra
import Utils

plot :: Building -> String
plot b = (plotVectors $ getEdgePoints $ edges b)
  ++ "\n\n" ++ (plotVectorList $ getItems diafani $ edges b)
  ++ "\n\n" ++ (plotVectorList $ getItems adiafani $ edges b)
  ++ "\n\n" ++ (plotVectorList $ getItems levels $ edges b)
  ++ "\n\n" ++ (plotVectorList $ ugetEpafes b)
  ++ "\n\n" ++ (plotVectorList $ getBalconies b)
  ++ "\n\n" ++ (plotVectorList $ getTents b)
  ++ "\n\n" ++ (plotVectorList $ getObstacles $ obstacles b)

plotPoint :: Vec -> String
plotPoint p = show (fst p) ++ " " ++ show (snd p) ++ "\n"

plotVectors :: [Vec] -> String
plotVectors vs = (foldr1 (++) $ map plotPoint vs) ++ "\n"

plotVectorList :: [[Vec]] -> String
plotVectorList [] = ""
plotVectorList vs = foldr1 (++) $ map plotVectors $ vs

-- plot skeleton
getEdgePoints :: [Edge] -> [Vec]
getEdgePoints = points . vectors

-- plot Items (diafani, adiafani, levels)
getItem :: Line -> Item a -> [Vec]
getItem (p, v) i = [p &+ ((fst $ fromTo i) &* v ), (p &+ ((snd $ fromTo i) &* v ))]

getEdgeItem :: (Edge -> [Item a]) -> Edge -> Vec -> [[Vec]]
getEdgeItem which e startPoint = map (getItem (startPoint, geom e)) (which e)

getItems :: (Edge -> [Item a]) -> [Edge] -> [[Vec]]
getItems which es = getItems' which es (getEdgePoints es)

getItems' :: (Edge -> [Item a]) -> [Edge] -> [Vec] -> [[Vec]]
getItems' which es ps = concat $ zipWith (getEdgeItem which) es ps

-- plot Epafes
getEpafi :: [Edge] -> Epafi -> [Vec]
getEpafi es ep = map (pointFromSkel es) pts
  where rep = reverseEpafi ep
        edg = [(fst $ epfStart rep) + 1 .. (fst $ epfEnd rep)]
        pts = [epfStart rep] ++ zip edg (repeat 0) ++ [epfEnd rep]

ugetEpafes :: Building -> [[Vec]]
ugetEpafes b = map (getEpafi (edges b)) (epafes b)

-- plot Balconies - tents
getBalcony :: Building -> Balcony -> [Vec]
getBalcony build balc = (startPoint : vecs) ++ [endPoint]
  where startPoint = pointFromSkel (edges build) $ balcStart balc
        endPoint = pointFromSkel (edges build) $ balcEnd balc
        vecs = scanl (&+) startPoint $ balcGeom balc

getBalconies :: Building -> [[Vec]]
getBalconies b = map (getBalcony b) $ balconies b
getTents b = map (getBalcony b) $ tents b

-- plot Obstacles
getObstacle :: Obstacle -> [Vec]
getObstacle o = [offset, offset &+ obstGeom o]
  where offset = obstOffset o

getObstacles :: [Obstacle] -> [[Vec]]
getObstacles = map getObstacle
