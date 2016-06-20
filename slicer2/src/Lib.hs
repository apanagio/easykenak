-- {-# LANGUAGE OverloadedStrings,TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    ) where


import Data.Aeson
import Data.Aeson.TH

import qualified Data.ByteString.Lazy.Char8 as BL

import DataStructure
import Algebra
import Utils
-- import Shadows

import qualified Plotter as PL

--temp
import Data.Vector (fromList, (!))
-- ~ import qualified Data.Vector as V
-- ~ import Data.Maybe(mapMaybe, catMaybes)
---

run :: IO ()
run = do
  building <- BL.getContents
  let b = eitherDecode building :: Either String Building
  case b of
    Left err -> putStrLn err
    Right d -> do
      writeFile "/tmp/ttt.dat" $ PL.plot $ nBuilding d
      let es = geom $ edges d
      let ls = zip (points es) es
      let v = fromList ls
      print $ getLength (geom $ edges d) (2, 0.5)
      -- ~ print $ getEdgeWithBalc (geom $ edges d) (head $ balconies d)
      -- print $ getShadowsEdge (edges d) (v ! 2 )
      -- print $ mergeShadows 2 (heightGross d) $ (getShadowsEdge (edges d) (v ! 2 ) ++ getShadowsObst (obstacles d) (v ! 2 ))
      print $ getAllShadows d
      -- print $ projectLine (v!2) (v!4)
      -- print $ zipWith tt (height $ edges d) $ map (projectLine (v!2)) ls
