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

-- ~ crossBorder :: [ParsedEdge] -> Balcony -> Bool
-- ~ crossBorder pe balc = 
