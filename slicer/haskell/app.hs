{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

import Data.Aeson
import Data.Aeson.TH

import qualified Data.ByteString.Lazy.Char8 as BL

import DataStructure
-- ~ import Algebra
import Utils
import Shadows

import Plotter

--temp
import qualified Data.Vector as V
---

main :: IO ()
main = do
  building <- BL.getContents
  let b = eitherDecode building :: Either String Building
  case b of
    Left err -> putStrLn err
    Right d -> do
      writeFile "/tmp/ttt.dat" $ plot d
      let c = getParsedEdges $ edges d
      let a = V.fromList $ c
      -- ~ putStrLn $ show $ pointFromSkel (edges d) (4, 1.0)
      -- ~ putStrLn $ show $ getEpafes d
      -- ~ print $ map len $ getParsedEdges $ edges d
      -- ~ print $ getShadowsFromEdge (a V.! 4) (getParsedEdges $ edges d)
      -- ~ print $ obstFromEdge (a V.! 4) (a V.! 1)
      -- ~ print $ getShadowsFromObst (a V.! 4) (obstacles d)
      -- ~ print $ mergeShadows (heightGross d) $ getAllShadows (a V.! 4) d
      -- ~ print $ getAllShadows (a V.! 4) d
      -- ~ print $ epafiFromEdge (a V.! 0) (head $ epafes d)
      print $ epafiFromEdge c (a V.! 0) (head $ epafes d)
      putStrLn "OK"
    
