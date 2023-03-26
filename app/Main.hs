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
{-# LANGUAGE TemplateHaskellQuotes #-}

module Main (main) where

import Data.Text
import Language.Haskell.TH
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
import LocalTimeTemplate

-- PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
--   User sql=users
--     name Text
--     email Text
--     age Int
--     occupation Text
--     -- test Text
--     UniqueEmail email
--     deriving Show Read
-- |]
data Config = Config {
  configConnect :: ConnectionString --BC.ByteString
}
PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  Person
    name String
  Store
    name String
  PersonStore
    personId PersonId
    storeId StoreId
    UniquePersonStore personId storeId
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
      let configDB = Config { configConnect = E.encodeUtf8 $ 
                              mconcat ["host=", cHost $ cfg
                                      ," port=", cPort $ cfg
                                      , " user=", cUser $ cfg
                                      , " dbname=", cDBname $ cfg
                                      , " password=", cPassword $ cfg]}
      migrateDB configDB 
      (\con -> runAction con $ do
        -- bruce <- insert $ Person "Bruce Wayne"
        -- michael <- insert $ Person "Michael"
        --
        -- target <- insert $ Store "Target"
        -- gucci <- insert $ Store "Gucci"
        -- sevenEleven <- insert $ Store "7-11"
        --
        -- insert $ PersonStore bruce gucci
        -- insert $ PersonStore bruce sevenEleven
        --
        -- insert $ PersonStore michael target
        -- insert $ PersonStore michael sevenEleven
        pure ()
        ) (configConnect configDB)
      -- insertUser configDB s
      print "YEEEEEEEEEEEEEESSSSSSSSSSS"
      printUser configDB "admin@test.com"
      -- print "Net"
      -- deleteUser configDB s
      -- printUser configDB "admin@test.com"
      -- deleteUser configDB news
      printUser configDB "mar@test.com"
      -- insertUser configDB news
      print "NOOOOOOOOOOOOOOOOOOOOOOOOO"
      printUser configDB "wa@test.com"
      -- printUser configDB "admin@test.com"
      -- updateUser configDB s
      -- printUser configDB "admin@test.com"
      -- printUser configDB "mar@test.com"
  putStrLn $ "LocalTime: " <> $(localtimeTemplate)
  pure ()


-- sampleUser :: Entity User
-- sampleUser = Entity (toSqlKey 1) $ User
--   { userName = "admin"
--   , userEmail = "admin@test.com"
--   , userAge = 23
--   , userOccupation = "System Administrator"
--   }

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
insertUser :: Config -> User -> IO ()
insertUser cfg user = runAction (configConnect cfg) $ do
  insert user
  liftIO $ do {print ( "insert\n"); print user}

deleteUser :: Config -> User -> IO ()
deleteUser cfg user = runAction (configConnect cfg) $ do
  deleteBy $ UniqueEmail (userEmail user)
  liftIO $ do {print ( "delete\n"); print user}

-- updateUser :: ConfigDB -> User -> IO ()
-- updateUser cfg user = runAction connectionCfg $ do
--   -- pid <- insert user
--   -- pid <- selectFirst $ UniqueEmail (userEmail user)
--   pid <- selectKeys [UserAge <. 20] []
--   -- update pid [UserAge =. 5]
--   liftIO $ do {print ( "insert\n"); print user}

type Email = Text


printUser :: Config -> Email -> IO ()
printUser cfg email = runAction (configConnect cfg) $ do
  r <- getBy $ UniqueEmail email
  -- r <- selectList [UserAge <. 30] []
  liftIO $ print $ r 

migrateDB :: Config -> IO ()
migrateDB cfg = runAction (configConnect cfg) $ do
  (runMigration migrateAll)
  -- insert s
  -- r <- getBy $ UniqueEmail "admin@test.com"
  -- liftIO $ print r 
-- migrateDB cfg = runAction connectionCfg (runMigrationUnsafe migrateAll)


-- selectYoungTeachers' :: (MonadIO m) => SqlPersistT m [Entity User]
-- selectYoungTeachers' = selectList
--   [UserAge <. 25, UserOccupation ==. "Teacher"] [Asc UserEmail, OffsetBy 5, LimitTo 100]

