{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

module DataStructure where

import Data.Aeson
import Data.Aeson.TH

import Data.Char(toLower)

import Algebra

-- input data
-- ~ type OnEdge = Double
type OnSkel = (Int, Double)

data Item a = Item {
  start :: OnSkel
  , end :: OnSkel
  , startHeight :: Interval
  , endHeight :: Interval
  , props :: a
} deriving (Eq, Show)

data Obstacle = Obstacle {
  obstOffset :: Vec
  , obstHeight :: Double
  , obstGeom :: Vec
} deriving (Eq, Show)

data Balcony = Balcony {
  balcHeight :: Double
  , balcStart :: OnSkel
  , balcEnd :: OnSkel
  , balcGeom :: [Vec]
} deriving (Eq, Show)

data Edges = Edges {
  geom :: [Vec]
  , height :: [Double]
  , specialObstacles :: [Double]
} deriving (Eq, Show)

data Building = Building {
  orientation :: Double
  , adiafaniType :: String
  , heightNet :: Double
  , heightGross :: Double
  , edges :: Edges
  , balconies :: [Balcony]
  , tents :: [Balcony]
  , epafes ::  [Item EpafiProps]
  , obstacles :: [Obstacle]
  , diafani :: [Item String]
  , adiafani :: [Item String]
  , levels :: [Item String]
  } deriving (Eq, Show)

-- intermediate data
data ObstacleProps = ObstacleProps {
  distance :: (Double, Double)
  , h :: Double
}  deriving Show

data BalconyProps = BalconyProps {
  parsedBalcLines :: [Line]
  , parsedBalcHeight :: Double
  , covers :: [(OnSkel, OnSkel)]
} deriving Show

type EpafiProps = Int
type ParsedObstacle = Item ObstacleProps
type ParsedBalcony = Item BalconyProps

--   edge :: Edge
--   , rank :: Int
--   , startPoint :: Vec
--   , len :: Double -- length of all edges before this one
--   , parsedObstacles :: [ParsedObstacle]
--   , parsedBalconies :: [ParsedBalcony]
--   , parsedTents :: [ParsedBalcony]
--   , parsedEpafes :: [ParsedEpafi]
-- } deriving Show
-- data ParsedEdge = ParsedEdge {

$(deriveJSON defaultOptions ''Edges)
$(deriveJSON defaultOptions ''Building)
$(deriveJSON defaultOptions ''Item)
$(deriveJSON defaultOptions{fieldLabelModifier = (\(x:xs) -> (toLower x:xs)) . drop 4 } ''Obstacle)
$(deriveJSON defaultOptions{fieldLabelModifier = (\(x:xs) -> (toLower x:xs)) . drop 4 } ''Balcony)
