{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

import Data.Aeson
import Data.Aeson.TH 
import Data.Aeson.Encode.Pretty

--import qualified Data.Map as Map
-- import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BL

data Edge = Edge {
	geom :: (Float, Float)
	, obstacles :: Maybe [Object]
	, specialObstacles :: Maybe Float
	, diafani :: Maybe [Object]
	, adiafani :: Maybe [Object]
	, levels :: Maybe [Object]
	}

data Building = Building {
	name :: String
	, orientation :: Float
	, adiafaniType :: Maybe String
	, height :: Maybe Float
	, heightNet :: Maybe Float
	, edges :: [Edge]
	, balconies :: Maybe [Object]
	, tents :: Maybe [String]
	, epafes :: Maybe [String]
	}

deriveJSON defaultOptions ''Edge
deriveJSON defaultOptions ''Building

-- slice :: Building -> a
slice b = encodePretty $ edges b

main :: IO ()
main = do
  
  building <- BL.getContents
  
  let b = eitherDecode building :: Either String Building 
  case b of 
    Left err -> putStrLn err
    Right d -> BL.putStrLn $ slice d
  
 
