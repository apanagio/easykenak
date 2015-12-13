{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

import Data.Aeson
import Data.Aeson.TH 
import Data.Aeson.Encode.Pretty
--import Control.Monad

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

--import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BL

import DataStructure

extractDiaf :: (Int, Edge) -> [Map String Float]
extractDiaf (i, d) = map (Map.insert "index" $ fromIntegral $ i) (diafani d)

getDiafani :: [Edge] -> [Map String Float]
getDiafani a = concat $ filter (not . null) (map extractDiaf $ zip [1 ..] a)

sliceLevel :: [Edge] -> [Edge]
slieceLevel 

-- slice :: Building -> a
slice b = edges b

main :: IO ()
main = do  
  building <- BL.getContents  
  let b = eitherDecode building :: Either String Building 
  case b of 
    Left err -> putStrLn err
    Right d -> putStrLn $ show  $  getDiafani $ slice d
  
 
