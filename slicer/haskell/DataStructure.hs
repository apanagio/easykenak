{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

module DataStructure 
( Epafi(..)
, Dispatched(..)
, Balcony(..)
, Edge(..)
, Building(..)
) where

import Data.Aeson
import Data.Aeson.TH 

import Data.Char

import qualified Data.Map.Strict as Map

data Epafi = Epafi {
	epfType :: Int
	, epfStart :: Map.Map String Float
	, epfEnd :: Map.Map String Float
	, epfStartHeight :: (Float, Float)
	, epfEndHeight :: (Float, Float)
} deriving (Eq, Show)

data Dispatched = Dispatched {
	dispOffset :: (Float, Float)
	, dispHeight :: Float
	, dispGeom :: (Float, Float)
} deriving (Eq, Show)

data Balcony = Balcony {
	balcHeight :: Float
	, balcStart :: Map.Map String Float
	, balcEnd :: Map.Map String Float
	, balcGeom :: [(Float, Float)]
} deriving (Eq, Show)

data Edge = Edge {
	geom :: (Float, Float)
	, specialObstacles :: Maybe Float
	, diafani :: [Map.Map String Float]
	, adiafani :: [Map.Map String Float]
	, levels :: [Map.Map String Float]
	} deriving (Eq, Show)

data Building = Building {
	name :: String
	, orientation :: Float
	, adiafaniType :: String
	, height :: Float
	, heightNet :: Float
	, edges :: [Edge]
	, balconies :: [Balcony]
	, epafes ::  [Epafi]
	} deriving (Eq, Show)

$(deriveJSON defaultOptions ''Edge)
$(deriveJSON defaultOptions ''Building)
$(deriveJSON defaultOptions{fieldLabelModifier = ( (\(x:xs) -> (toLower x:xs)) . drop 4 )} ''Dispatched)
$(deriveJSON defaultOptions{fieldLabelModifier = ( (\(x:xs) -> (toLower x:xs)) . drop 4 )} ''Balcony)
$(deriveJSON defaultOptions{fieldLabelModifier = ( (\(x:xs) -> (toLower x:xs)) . drop 3)} ''Epafi)
