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
-- import Utils
-- import Shadows
-- import Epafes
-- import Balconies

-- import qualified Plotter as PL

--temp
import qualified Data.Vector as V
---

run :: IO ()
run = do
  building <- BL.getContents
  let b = eitherDecode building :: Either String Building
  case b of
    Left err -> putStrLn err
    Right d -> do
  --     writeFile "/tmp/ttt.dat" $ PL.plot d
  --     let c = getParsedEdges $ edges d
  --     let a = V.fromList c
      print d
