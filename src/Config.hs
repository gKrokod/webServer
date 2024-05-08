{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Config where

import qualified Data.Text as T
import Data.Aeson (eitherDecode, encode, ToJSON(..), FromJSON (..))
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as L 
import qualified Data.Text.Encoding as E (encodeUtf8)
import Database.Persist.Postgresql (ConnectionString)
import Control.Exception -- (try, Exception, catch)
import Control.Monad (join)
import System.IO.Error

data ConfigDataBase = MkConfigDataBase {
    cHostDB :: T.Text
  , cPortDB :: T.Text
  , cUserDB :: T.Text
  , cNameDB :: T.Text
  , cPasswordDB :: T.Text,
    cLimitData :: Int,
    cPortServer :: Int
} deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- readConf :: IO (Either String L.ByteString)
-- readConf :: IO (Either String ConfigDataBase)
-- readConf :: IO (Either String ConfigDataBase)
readConf :: IO (Either SomeException ConfigDataBase)
-- readConf = either (Left . userError) (Right) <$> eitherDecode <$> L.readFile "config/db3.cfg"
readConf = either (\_ -> (Left $ toException NonTermination)) (Right) <$> eitherDecode <$> L.readFile "config/db.cfg"



--for work 
-- loadConfigDB :: IO (Either String ConfigDataBase)
-- loadConfigDB :: IO (Either String L.ByteString)
-- loadConfigDB :: IO (Either SomeException ConfigDataBase)
loadConfigDB :: IO (Either SomeException ConfigDataBase)
loadConfigDB = join <$> try (
    either (\_ -> (Left $ toException NonTermination)) Right 
    <$> eitherDecode 
    <$> L.readFile "config/db.cfg")
  

--     txt <- L.readFile "config/db3.cfg"
--     let r = eitherDecode  txt 
--     pure r
                  -- (eitherDecode <$> L.readFile "config/db3.cfg")

--  (fmap eitherDecode) (try $ L.readFile "config/db3.cfg")

-- configDB :: IO (Either String ConnectionString)
-- configDB = do
--   config <- loadConfigDB
--   case config of 
--     Left decodeError -> pure $ Left decodeError
--     Right cfg -> pure $ Right $
--                    E.encodeUtf8 $ 
--                      mconcat ["host=", cHostDB $ cfg
--                              ," port=", cPortDB $ cfg
--                              , " user=", cUserDB $ cfg
--                              , " dbname=", cNameDB $ cfg
--                              , " password=", cPasswordDB $ cfg]
-- for testing
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
  } 
  let configToJSON = encode testConfig :: L.ByteString
  L.writeFile "config/db.cfg" (configToJSON)
