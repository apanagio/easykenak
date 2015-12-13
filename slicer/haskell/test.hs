{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as BL

data Address = Address
    { house  :: Integer
    , street :: String
    , city   :: String
    , state  :: Maybe String
    , zip    :: Integer
    } deriving (Show, Eq)

data Person = Person
    { name    :: String
    , age     :: Integer
    , address :: Address
    } deriving (Show, Eq)

$(deriveJSON defaultOptions ''Address)
$(deriveJSON defaultOptions ''Person)

aa :: BL.ByteString
aa = "{\"name\": \"some body\", \"age\" : 23, \"address\" : {\"house\" : 285, \"street\" : \"7th Ave.\", \"city\" : \"New York\", \"state\" : \"New York\", \"zip\" : 10001}}"

main = print (decode aa :: Maybe Person)
