{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Config (loadConfigDB, ConfigDB(..)) where

import qualified Data.Text as T
import Data.Aeson (eitherDecode, encode, ToJSON(..), FromJSON (..))
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Lazy as L 


data ConfigDB = ConfigDB {
    cHost :: T.Text
  , cPort :: T.Text
  , cUser :: T.Text
  , cDBname :: T.Text
  , cPassword :: T.Text
} deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--for work 
loadConfigDB :: IO (Either String ConfigDB)
loadConfigDB =  eitherDecode <$> L.readFile "config/db.conf"

-- for testing
createConfigFile :: IO ()
createConfigFile = do
  let testConfig = ConfigDB {
      cHost = ""
    , cPort = ""
    , cUser = ""
    , cDBname = ""
    , cPassword = ""
  } 
  let configToJSON = encode testConfig :: BC.ByteString
  L.writeFile "config/db.conf" (configToJSON)

