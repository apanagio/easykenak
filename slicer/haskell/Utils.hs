module Utils where

import Algebra
import DataStructure

data ParsedObstacle = ParsedObstacle {
  start :: OnEdge
  , end :: OnEdge
  , angle :: Double
} deriving Show
  
shadow :: Line -> Obstacle -> [ParsedObstacle]
shadow l o = [ParsedObstacle {
  start = 0.2
  , end = 0.8
  , angle = 45.2
}]
   
