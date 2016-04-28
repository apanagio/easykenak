{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Encode.Pretty

import Data.Map.Strict (Map)
-- ~ import qualified Data.Map.Strict as Map

import qualified Data.ByteString.Lazy.Char8 as BL

-- ~ import Data.Vect.Float

import DataStructure
import Algebra
import Utils

import Graphics.Gnuplot.Simple

main :: IO ()
main = do
  building <- BL.getContents
  let b = eitherDecode building :: Either String Building
  case b of
    Left err -> putStrLn err
    Right d -> do 
      plotList [] [(1.0 :: Double, 1.0 :: Double), (2.0, 2.0), (3.0, 3.0)]
    
