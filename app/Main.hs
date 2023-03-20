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
    -- test Text
    UniqueEmail email
    deriving Show Read
|]

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
      insertUser cfg s
      print "Est"
      printUser cfg "admin@test.com"
      print "Net"
      deleteUser cfg s
      printUser cfg "admin@test.com"
      -- insertUser cfg news
      -- printUser cfg "wa@test.com"
      -- printUser cfg "admin@test.com"
      -- updateUser cfg s
      -- printUser cfg "admin@test.com"
      -- printUser cfg "mar@test.com"
  pure ()


sampleUser :: Entity User
sampleUser = Entity (toSqlKey 1) $ User
  { userName = "admin"
  , userEmail = "admin@test.com"
  , userAge = 23
  , userOccupation = "System Administrator"
  }

s:: User
s=  User
  { userName = "admin"
  , userEmail = "admin@test.com"
  , userAge = 23
  , userOccupation = "System Administrator"
  }

news:: User
news=  User
  { userName = "mar"
  , userEmail = "mar@test.com"
  , userAge = 33
  , userOccupation = "vodka"
  }
  
runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a ->  IO a
runAction connectionString action = runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
  runReaderT action backend
--
insertUser :: ConfigDB -> User -> IO ()
insertUser cfg user = runAction connectionCfg $ do
  insert user
  liftIO $ do {print ( "insert\n"); print user}
  where connectionCfg = E.encodeUtf8 $ mconcat ["host=", cHost $ cfg
                                               ," port=", cPort $ cfg
                                               , " user=", cUser $ cfg
                                               , " dbname=", cDBname $ cfg
                                               , " password=", cPassword $ cfg]

deleteUser :: ConfigDB -> User -> IO ()
deleteUser cfg user = runAction connectionCfg $ do
  deleteBy $ UniqueEmail (userEmail user)
  liftIO $ do {print ( "delete\n"); print user}
  where connectionCfg = E.encodeUtf8 $ mconcat ["host=", cHost $ cfg
                                               ," port=", cPort $ cfg
                                               , " user=", cUser $ cfg
                                               , " dbname=", cDBname $ cfg
                                               , " password=", cPassword $ cfg]
-- updateUser :: ConfigDB -> User -> IO ()
-- updateUser cfg user = runAction connectionCfg $ do
--   -- pid <- insert user
--   -- pid <- selectFirst $ UniqueEmail (userEmail user)
--   pid <- selectKeys [UserAge <. 20] []
--   -- update pid [UserAge =. 5]
--   liftIO $ do {print ( "insert\n"); print user}
--   where connectionCfg = E.encodeUtf8 $ mconcat ["host=", cHost $ cfg
--                                                ," port=", cPort $ cfg
--                                                , " user=", cUser $ cfg
--                                                , " dbname=", cDBname $ cfg
--                                                , " password=", cPassword $ cfg]
type Email = Text

printUser :: ConfigDB -> Email -> IO ()
printUser cfg email = runAction connectionCfg $ do
  r <- getBy $ UniqueEmail email
  -- r <- selectList [UserAge <. 30] []
  liftIO $ print $ r 
  where connectionCfg = E.encodeUtf8 $ mconcat ["host=", cHost $ cfg
                                               ," port=", cPort $ cfg
                                               , " user=", cUser $ cfg
                                               , " dbname=", cDBname $ cfg
                                               , " password=", cPassword $ cfg]

migrateDB :: ConfigDB -> IO ()
migrateDB cfg = runAction connectionCfg $ do
  (runMigration migrateAll)
  -- insert s
  -- r <- getBy $ UniqueEmail "admin@test.com"
  -- liftIO $ print r 
-- migrateDB cfg = runAction connectionCfg (runMigrationUnsafe migrateAll)
  where connectionCfg = E.encodeUtf8 $ mconcat ["host=", cHost $ cfg
                                               ," port=", cPort $ cfg
                                               , " user=", cUser $ cfg
                                               , " dbname=", cDBname $ cfg
                                               , " password=", cPassword $ cfg]


-- selectYoungTeachers' :: (MonadIO m) => SqlPersistT m [Entity User]
-- selectYoungTeachers' = selectList
--   [UserAge <. 25, UserOccupation ==. "Teacher"] [Asc UserEmail, OffsetBy 5, LimitTo 100]

