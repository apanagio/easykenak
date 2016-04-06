{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

module DataStructure where

import Data.Aeson
import Data.Aeson.TH 

import Data.Char(toLower)

import Algebra

type OnEdge = Double
type OnSkel = (Int, OnEdge)

data Epafi = Epafi {
	epfType :: Int
	, epfStart :: OnSkel
	, epfEnd :: OnSkel
	, epfStartHeight :: (Double, Double)
	, epfEndHeight :: (Double, Double)
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

data Item = Item {
    itemStart :: OnEdge
    , itemEnd :: OnEdge
    , itemStartHeight :: (Double, Double)
    , itemEndHeight :: (Double, Double)
    , itemProps :: String
} deriving (Eq, Show)

data Edge = Edge {
	geom :: Vec
    , height :: Double
	, specialObstacles :: Double
	, diafani :: [Item]
	, adiafani :: [Item]
	, levels :: [Item]
	} deriving (Eq, Show)

data Building = Building {
	orientation :: Double
	, adiafaniType :: String
	, heightNet :: Double
	, edges :: [Edge]
	, balconies :: [Balcony]
    , tents :: [Balcony]
	, epafes ::  [Epafi]
    , obstacles :: [Obstacle]
	} deriving (Eq, Show)

$(deriveJSON defaultOptions ''Edge)
$(deriveJSON defaultOptions ''Building)
$(deriveJSON defaultOptions{fieldLabelModifier = ( (\(x:xs) -> (toLower x:xs)) . drop 4 )} ''Item)
$(deriveJSON defaultOptions{fieldLabelModifier = ( (\(x:xs) -> (toLower x:xs)) . drop 4 )} ''Obstacle)
$(deriveJSON defaultOptions{fieldLabelModifier = ( (\(x:xs) -> (toLower x:xs)) . drop 4 )} ''Balcony)
$(deriveJSON defaultOptions{fieldLabelModifier = ( (\(x:xs) -> (toLower x:xs)) . drop 3)} ''Epafi)
