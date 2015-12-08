{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}
import Debug.Trace

import Data.Aeson
import Data.Aeson.TH 
import Data.Aeson.Encode.Pretty


import qualified Data.Map(Map)
import qualified Data.Map as Map

import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BL

import DataStructure


-- slice :: Building -> a
slice b = edges b

main :: IO ()
main = do
  
  building <- BL.getContents
  
  let b = eitherDecode building :: Either String Building 
  case b of 
    Left err -> putStrLn err
    Right d -> putStrLn $ show $ slice d
  
 
