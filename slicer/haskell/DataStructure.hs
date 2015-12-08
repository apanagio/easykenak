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

import qualified Data.Map(Map)
import qualified Data.Map as Map


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
	, obstacles :: Maybe [Dispatched]
	, tents:: Maybe [Dispatched]
	, specialObstacles :: Maybe Float
	, diafani :: Maybe [Map.Map String Float]
	, adiafani :: Maybe [Map.Map String Float]
	, levels :: Maybe [Map.Map String Float]
	} deriving (Eq, Show)

data Building = Building {
	name :: String
	, orientation :: Float
	, adiafaniType :: Maybe String
	, height :: Maybe Float
	, heightNet :: Maybe Float
	, edges :: [Edge]
	, balconies :: Maybe [Balcony]
	, epafes :: Maybe [Epafi]
	} deriving (Eq, Show)

$(deriveJSON defaultOptions ''Edge)
$(deriveJSON defaultOptions ''Building)
$(deriveJSON defaultOptions{fieldLabelModifier = ( (\(x:xs) -> (toLower x:xs)) . drop 4 )} ''Dispatched)
$(deriveJSON defaultOptions{fieldLabelModifier = ( (\(x:xs) -> (toLower x:xs)) . drop 4 )} ''Balcony)
$(deriveJSON defaultOptions{fieldLabelModifier = ( (\(x:xs) -> (toLower x:xs)) . drop 3)} ''Epafi)
