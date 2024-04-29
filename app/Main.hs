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
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT, runStderrLoggingT, LoggingT(..), runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import qualified Data.Text as T
import qualified Database.Persist.TH as PTH
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlConn) 
import Database.Persist.Postgresql  (SqlPersistT,ConnectionString, insert, runMigration, runSqlPersistMPool, withPostgresqlPool, withPostgresqlConn)
import Config (loadConfigDB, ConfigDB(..))
import qualified Data.Text.Encoding as E (encodeUtf8)

import LocalTimeTemplate (localtimeTemplate)
-- import Base.Base (Config(..), migrateDB, runAction)
import Base.Base 
import Base.BasicSchema
-- import Base.Base (Config(..))

main :: IO ()
main = do
  Prelude.putStrLn "Main start"
  db <- configDB
  case db of
    Nothing -> pure ()
    Just db' -> doLogic $ configConnect db'
  -- pure ()

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend

migrateDB :: ConnectionString -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)
--

-- runDBaction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
-- runDBaction pginfo action = runStdoutLoggingT $ withPostgresqlConn pginfo $ \backend -> runSqlConn action backend
runDBaction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runDBaction pginfo a = runStdoutLoggingT $ withPostgresqlConn pginfo $ \backend -> runSqlConn a backend 
-- runDBaction pginfo = runStderrLoggingT . withPostgresqlPool pginfo 10 . \pool -> liftIO 

  -- runStderrLoggingT $ withPostgresqlConn pginfo $ \backend ->
  -- runNoLoggingT $ withPostgresqlConn pginfo $ \backend ->
    -- flip runReaderT backend $ do
    -- flip runSqlConn backend $ do



doLogic :: ConnectionString -> IO ()
doLogic pginfo = do
  runDBaction pginfo $ runMigration migrateAll
  -- migrateDB pginfo
  putStrLn $ "LocalTime: " <> $(localtimeTemplate)
  -- runNoLoggingT $ withPostgresqlConn pginfo $ \backend ->  
  --  flip runSqlConn backend 
  -- runStdoutLoggingT $ withPostgresqlConn pginfo $ \backend ->
  -- runStderrLoggingT $ withPostgresqlConn pginfo $ \backend ->
  -- runNoLoggingT $ withPostgresqlConn pginfo $ \backend ->
    -- flip runReaderT backend $ do
    -- flip runSqlConn backend _
    -- flip runSqlConn backend $ do
  runDBaction pginfo $ do

  --
  -- -- runNoLoggingT $ withPostgresqlPool pginfo 10 $ \pool -> liftIO $ do
  -- -- runStderrLoggingT $ withPostgresqlPool pginfo 10 $ \pool -> liftIO $ do
  --  -- flip runSqlPersistMPool pool $ do
  --  -- runDBaction pginfo $ do 
      bruce <- insert $ Person "Bruce Wayne"
  --     -- pure ()
      michael <- insert $ Person "Michael"
      target <- insert $ Store "Target"
      gucci <- insert $ Store "Gucci"
      sevenEleven <- insert $ Store "7-11"

      insert $ PersonStore bruce gucci
      insert $ PersonStore bruce sevenEleven

      insert $ PersonStore michael target
      insert $ PersonStore michael sevenEleven
      pure ()
  -- pure ()
-- загружаем конфиг для подключения к постгрес
--
configDB :: IO (Maybe Config)
configDB = do
  config <- loadConfigDB
  case config of 
    Left decodeError -> print decodeError >> pure Nothing
    Right cfg -> do
      print "Config DataBase is loaded"
      let configDB = Config { configConnect = E.encodeUtf8 $ 
                              mconcat ["host=", cHost $ cfg
                                      ," port=", cPort $ cfg
                                      , " user=", cUser $ cfg
                                      , " dbname=", cDBname $ cfg
                                      , " password=", cPassword $ cfg]}
      pure $ Just configDB
