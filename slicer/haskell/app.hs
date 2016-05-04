{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

import Data.Aeson
import Data.Aeson.TH


import qualified Data.ByteString.Lazy.Char8 as BL

import DataStructure
import Algebra
import Utils
import Plotter

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
    
