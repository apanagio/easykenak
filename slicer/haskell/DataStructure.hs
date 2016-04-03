{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

module DataStructure 
( Epafi(..)
, Obstacle(..)
, Balcony(..)
, Edge(..)
, Building(..)
) where

import Data.Aeson
import Data.Aeson.TH 

import Data.Char(toLower)

import qualified Data.Map.Strict as Map

type OnEdge = Float
type OnSkel = (Int, OnEdge)

type Vec = (Float, Float)

data Epafi = Epafi {
	epfType :: Int
	, epfStart :: OnSkel
	, epfEnd :: OnSkel
	, epfStartHeight :: (Float, Float)
	, epfEndHeight :: (Float, Float)
} deriving (Eq, Show)

data Obstacle = Obstacle {
	obstOffset :: Vec
	, obstHeight :: Float
	, obstGeom :: Vec
} deriving (Eq, Show)

data Balcony = Balcony {
	balcHeight :: Float
	, balcStart :: OnSkel
	, balcEnd :: OnSkel
	, balcGeom :: [Vec]
} deriving (Eq, Show)

data Item = Item {
    itemStart :: OnEdge
    , itemEnd :: OnEdge
    , itemStartHeight :: (Float, Float)
    , itemEndHeight :: (Float, Float)
    , itemProps :: String
} deriving (Eq, Show)

data Edge = Edge {
	geom :: Vec
    , height :: Float
	, specialObstacles :: Float
	, diafani :: [Item]
	, adiafani :: [Item]
	, levels :: [Item]
	} deriving (Eq, Show)

data Building = Building {
	orientation :: Float
	, adiafaniType :: String
	, heightNet :: Float
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
