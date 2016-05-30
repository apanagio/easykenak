module Epafes where

import qualified Data.Vector as V
import Data.Maybe (mapMaybe)

import Algebra
import DataStructure
import Utils

-- reverse epafi if start > end
reverseEpafi :: Epafi -> Epafi
reverseEpafi ep = ep {epfStart = min (epfEnd ep) (epfStart ep), epfEnd = max (epfEnd ep) (epfStart ep)}

-- returns the slope of epafi (bottom slope, top slope)
slope :: [ParsedEdge] -> Epafi -> Vec
slope edgeList epf = (1/epfLen) &* (epfEndHeight epf &- epfStartHeight epf)
  where epfLen = getLength edgeList (epfEnd epf) - getLength edgeList (epfStart epf)

-- needs sorted epafi
epafiFromEdge :: [ParsedEdge] -> ParsedEdge -> Epafi -> Maybe ParsedEpafi
epafiFromEdge edgeList e epf
  | rank e < fst (epfStart epf) = Nothing
  | rank e > fst (epfEnd epf) = Nothing
  | otherwise = Just Item {
    fromTo = (a, b)
    , startHeight = epfStartHeight epf &+ (startLen &* sl)
    , endHeight = epfStartHeight epf &+ (endLen &* sl)
    , props = epfType epf
  }
  where a = if rank e > fst (epfStart epf) then 0 else snd $ epfStart epf
        b = if rank e < fst (epfEnd epf) then 1 else snd $ epfEnd epf
        sl = slope edgeList epf
        startLen = getLength edgeList (rank e, a) - getLength edgeList (epfStart epf)
        endLen = getLength edgeList (rank e, b) - getLength edgeList (epfStart epf)

--gets all epafes for specified edge
getEpafes :: Building -> ParsedEdge -> [ParsedEpafi]
getEpafes b pe = mapMaybe ( epafiFromEdge (getParsedEdges $ edges b) pe . reverseEpafi) $ epafes b

-- Adds all epafes to edge
addEpafes :: Building -> ParsedEdge -> ParsedEdge
addEpafes b pe = pe {parsedEpafes = getEpafes b pe}
