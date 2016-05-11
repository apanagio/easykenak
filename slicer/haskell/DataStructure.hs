{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

module DataStructure where

import Data.Aeson
import Data.Aeson.TH 

import Data.Char(toLower)

import Algebra

-- input data
type OnEdge = Double
type OnSkel = (Int, OnEdge)

data Epafi = Epafi {
  epfType :: Int
  , epfStart :: OnSkel
  , epfEnd :: OnSkel
  , epfStartHeight :: Interval
  , epfEndHeight :: Interval
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

data Item a = Item {
  fromTo :: Interval
  , startHeight :: Interval
  , endHeight :: Interval
  , props :: a
} deriving (Eq, Show)

data Edge = Edge {
  geom :: Vec
  , height :: Double
  , specialObstacles :: Double
  , diafani :: [Item String]
  , adiafani :: [Item String]
  , levels :: [Item String]
  } deriving (Eq, Show)

data Building = Building {
  orientation :: Double
  , adiafaniType :: String
  , heightNet :: Double
  , heightGross :: Double
  , edges :: [Edge]
  , balconies :: [Balcony]
  , tents :: [Balcony]
  , epafes ::  [Epafi]
  , obstacles :: [Obstacle]
  } deriving (Eq, Show)
  
-- intermediate data
data ObstacleProps = ObstacleProps {
  distance :: (Double, Double)
  , h :: Double
}  deriving Show

type EpafiProps = Int

type ParsedObstacle = Item ObstacleProps
type ParsedEpafi = Item EpafiProps

data ParsedEdge = ParsedEdge {
  edge :: Edge
  , rank :: Int
  , startPoint :: Vec
  , len :: Double -- length of all edges before this one
  , parsedObstacles :: [ParsedObstacle]
  , parsedBalconies :: [ParsedObstacle]
  , parsedTents :: [ParsedObstacle]
  , parsedEpafes :: [ParsedEpafi]
} deriving Show

$(deriveJSON defaultOptions ''Edge)
$(deriveJSON defaultOptions ''Building)
$(deriveJSON defaultOptions ''Item)
$(deriveJSON defaultOptions{fieldLabelModifier = ( (\(x:xs) -> (toLower x:xs)) . drop 4 )} ''Obstacle)
$(deriveJSON defaultOptions{fieldLabelModifier = ( (\(x:xs) -> (toLower x:xs)) . drop 4 )} ''Balcony)
$(deriveJSON defaultOptions{fieldLabelModifier = ( (\(x:xs) -> (toLower x:xs)) . drop 3)} ''Epafi)
