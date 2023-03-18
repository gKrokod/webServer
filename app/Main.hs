{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DerivingStrategies #-}
--
--
-- {-# LANGUAGE MultiParamTypeClasses      #-}
-- {-# LANGUAGE GADTs                      #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE RecordWildCards            #-}
-- {-# LANGUAGE FlexibleInstances          #-}
-- {-# LANGUAGE StandaloneDeriving         #-}

module Main (main) where

import Data.Text
-- import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Logger 
import qualified Database.Persist.TH as PTH
-- import Database.Persist 
import Database.Persist.Postgresql 
import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Config (loadConfigDB, ConfigDB(..))
-- import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text.Encoding as E (encodeUtf8)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User sql=users
    name Text
    email Text
    age Int
    occupation Text
    UniqueEmail email
    deriving Show Read
|]

sampleUser :: Entity User
sampleUser = Entity (toSqlKey 1) $ User
  { userName = "admin"
  , userEmail = "admin@test.com"
  , userAge = 23
  , userOccupation = "System Administrator"
  }
  
runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a ->  IO a
runAction connectionString action = runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
  runReaderT action backend
--
migrateDB :: ConfigDB -> IO ()
migrateDB cfg = runAction connectionCfg (runMigration migrateAll)
  where connectionCfg = E.encodeUtf8 $ mconcat ["host=", cHost $ cfg
                                               ," port=", cPort $ cfg
                                               , " user=", cUser $ cfg
                                               , " dbname=", cDBname $ cfg
                                               , " password=", cPassword $ cfg]


-- selectYoungTeachers' :: (MonadIO m) => SqlPersistT m [Entity User]
-- selectYoungTeachers' = selectList
--   [UserAge <. 25, UserOccupation ==. "Teacher"] [Asc UserEmail, OffsetBy 5, LimitTo 100]

main :: IO ()
main = do
  Prelude.putStrLn "Main start"
  config <- loadConfigDB
  case config of 
    Left decodeError -> print decodeError 
    Right cfg -> do
      print "Just cfg"
      print cfg
      migrateDB cfg
  pure ()

