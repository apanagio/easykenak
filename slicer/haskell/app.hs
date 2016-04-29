{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

import Data.Aeson
import Data.Aeson.TH
-- ~ import Data.Aeson.Encode.Pretty

-- ~ import Data.Map.Strict (Map)
-- ~ import qualified Data.Map.Strict as Map

import qualified Data.ByteString.Lazy.Char8 as BL

-- ~ import Data.Vect.Float

import DataStructure
import Algebra
import Utils
import Plotter

-- ~ import Graphics.Gnuplot.Simple

main :: IO ()
main = do
  building <- BL.getContents
  let b = eitherDecode building :: Either String Building
  case b of
    Left err -> putStrLn err
    Right d -> do
      writeFile "/tmp/ttt.dat" $ plot d
      -- ~ putStrLn $ show $ pointFromSkel (edges d) (4, 1.0)
      -- ~ putStrLn $ show $ getEpafes d
      putStrLn "OK"
    
