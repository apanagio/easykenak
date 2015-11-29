-- This example is basically the same as in Simplest.hs, only it uses
-- GHC's builtin generics instead of explicit instances of ToJSON and
-- FromJSON.

-- We enable the DeriveGeneric language extension so that GHC can
-- automatically derive the Generic class for us.

{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (FromJSON, ToJSON, decode, encode, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BL

-- To decode or encode a value using the generic machinery, we must
-- make the type an instance of the Generic class.

import GHC.Generics (Generic)

data Obstacle = Obstacle {
    offset:: [Double]
    , height:: Double
    , obstacle_geom:: [Double]
    } deriving (Generic, Show)
    
data Edge = Edge {
    edge_geom:: [Double]
    , obstacles:: [Obstacle]
    } deriving (Generic, Show)

data Building = Building {
    name ::          String
    , orientation :: Int
    , edges ::       [Edge]
    } deriving (Generic, Show)

-- While we still have to declare our type as instances of FromJSON
-- and ToJSON, we do *not* need to provide bodies for the instances.
-- Default versions will be supplied for us.


instance FromJSON Building
instance ToJSON Building

main :: IO ()
main = do
  
  building <- BL.getContents
  
  let b = eitherDecode building :: Either String Building
  print b  
