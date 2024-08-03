{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

-- module Config (ConfigDataBase (..), loadConfig, connectionString, whenMakeTables) where
module Config (ConfigDataBase (..), loadConfig, connectionString, whenMakeTables, createConfigFile) where

import Control.Exception (SomeException, displayException, throwIO, try)
import Control.Monad (when)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode, encode)
import qualified Data.ByteString.Lazy as L
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E (encodeUtf8)
import Database.Persist.Postgresql (ConnectionString)
import GHC.Generics (Generic)
import Handlers.Logger

data ConfigDataBase = MkConfigDataBase
  { cHostDB :: T.Text,
    cPortDB :: T.Text,
    cUserDB :: T.Text,
    cNameDB :: T.Text,
    cPasswordDB :: T.Text,
    cLimitData :: Int,
    cPortServer :: Int,
    cLogLvl :: Log,
    cCreateAndFillTable :: Maybe DoIt
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DoIt = DoIt
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

whenMakeTables :: (Applicative f) => ConfigDataBase -> f () -> f ()
whenMakeTables = when . isJust . cCreateAndFillTable

loadConfigDB :: IO (Either String ConfigDataBase)
loadConfigDB =
  either (Left . displayException) eitherDecode
    <$> try @SomeException (L.readFile "config/db.cfg")

loadConfig :: IO ConfigDataBase
loadConfig = do
  cfg <- loadConfigDB
  case cfg of
    Left error' -> throwIO $ userError error'
    Right config -> pure config

-- info for connect witj postgres
connectionString :: ConfigDataBase -> ConnectionString
connectionString cfg =
  E.encodeUtf8 $
    mconcat
      [ "host=",
        cHostDB cfg,
        " port=",
        cPortDB cfg,
        " user=",
        cUserDB cfg,
        " dbname=",
        cNameDB cfg,
        " password=",
        cPasswordDB cfg
      ]

-- -- for testing
createConfigFile :: IO ()
createConfigFile = do
  let testConfig = MkConfigDataBase {
      cHostDB = "127.0.0.1"
    , cPortDB = "5432"
    , cUserDB = "bob"
    , cNameDB = "bobdb"
    , cPasswordDB = "1"
    , cLimitData = 5
    , cPortServer = 4221
    , cLogLvl = Debug
    , cCreateAndFillTable = Nothing --Just DoIt
    -- , cCreateAndFillTable = Just DoIt --Nothing --Just DoIt
  }
  let configToJSON = encode testConfig :: L.ByteString
  L.writeFile "config/db1.cfg" configToJSON
