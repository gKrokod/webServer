{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Config where

import qualified Data.Text as T
import Data.Aeson (eitherDecode, encode, ToJSON(..), FromJSON (..))
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as L 
import qualified Data.Text.Encoding as E (encodeUtf8)
import Database.Persist.Postgresql (ConnectionString)

data ConfigDataBase = MkConfigDataBase {
    cHost :: T.Text
  , cPort :: T.Text
  , cUser :: T.Text
  , cDBname :: T.Text
  , cPassword :: T.Text
} deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--for work 
loadConfigDB :: IO (Either String ConfigDataBase)
loadConfigDB =  eitherDecode <$> L.readFile "config/db.cfg"

configDB :: IO (Either String ConnectionString)
configDB = do
  config <- loadConfigDB
  case config of 
    Left decodeError -> pure $ Left decodeError
    Right cfg -> pure $ Right $
                   E.encodeUtf8 $ 
                     mconcat ["host=", cHost $ cfg
                             ," port=", cPort $ cfg
                             , " user=", cUser $ cfg
                             , " dbname=", cDBname $ cfg
                             , " password=", cPassword $ cfg]
-- for testing
createConfigFile :: IO ()
createConfigFile = do
  let testConfig = MkConfigDataBase {
      cHost = "127.0.0.1"
    , cPort = "5432"
    , cUser = "bob"
    , cDBname = "bobdb"
    , cPassword = "1"
  } 
  let configToJSON = encode testConfig :: L.ByteString
  L.writeFile "config/db1.cfg" (configToJSON)
