{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds       #-}

module Main (main) where

import Data.Text
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Logger 

import qualified Database.Persist.TH as PTH
import Database.Persist 
import Database.Persist.Postgresql 

connString :: ConnectionString
connString = "host=127.0.0.1 port=5432 user= dbname= password="

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User sql=users
    name Text
    email Text
    age Int
    occupation Text
    UniqueEmail email
    deriving Show Read
|]

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a ->  IO a
runAction connectionString action = runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
  runReaderT action backend
--
migrateDB :: IO ()
migrateDB = runAction connString (runMigration migrateAll)
--
-- sampleUser :: Entity User
-- sampleUser = Entity (toSqlKey 1) $ User
--   { userName = "admin"
--   , userEmail = "admin@test.com"
--   , userAge = 23
--   , userOccupation = "System Administrator"
--   }

-- data User = User
--   { userName :: Text
--   , userEmail :: Text
--   , userAge :: Int
--   , userOccupation :: Text
--   }

-- create table users (
--    name varchar(100),
--    email varchar(100),
--    age bigint,
--    occupation varchar(100)
-- )

-- selectYoungTeachers' :: (MonadIO m) => SqlPersistT m [Entity User]
-- selectYoungTeachers' = selectList
--   [UserAge <. 25, UserOccupation ==. "Teacher"] [Asc UserEmail, OffsetBy 5, LimitTo 100]

main :: IO ()
main = do
  putStrLn "132"
  migrateDB 
  putStrLn "ddd"
  let b = "Petro"
  putStrLn b
  pure ()

