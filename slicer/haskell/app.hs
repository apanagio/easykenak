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

main :: IO ()
main = do
  building <- BL.getContents
  let b = eitherDecode building :: Either String Building
  case b of
    Left err -> putStrLn err
    Right d -> do 
      print $ obstacles d
      let a = q 0 0
          b = q 0 2
          c = q 1 1
          d = q 2 3
          l1 = q a b
          l2 = q c d
      print $ project a l2
    
