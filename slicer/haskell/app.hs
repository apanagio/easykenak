{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

import Data.Aeson
import Data.Aeson.TH 
--import qualified Data.Map as Map
-- import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BL
--import GHC.Generics


data Building = Building {
	name :: String
	, orientation :: Float
	, area :: Maybe Float
	} deriving (Show, Eq)


$(deriveJSON defaultOptions ''Building)

-- slice :: Building -> a
slice b = encode b

main :: IO ()
main = do
  
  building <- BL.getContents
  
  let b = eitherDecode building :: Either String Building 
  case b of 
    Left err -> putStrLn err
    Right d -> print $ slice d
  
  --putStrLn (getResult b)
 
