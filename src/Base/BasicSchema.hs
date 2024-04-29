{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
-- for ToJSON and FromJSON
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
-- for json with Persist
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE StandaloneDeriving #-}

module Base.BasicSchema where
import Data.Aeson
import Data.Aeson.Types
import Database.Persist 

import Control.Monad.Logger (runNoLoggingT, runStderrLoggingT, LoggingT(..), runStdoutLoggingT, NoLoggingT(..))
import Control.Monad.IO.Class
import qualified Database.Persist.TH as PTH
-- import qualified Database.Persist.Sql as PS
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlConn) 
import Database.Persist.Postgresql  (rawExecute, SqlPersistT,ConnectionString, insert, runMigration, runSqlPersistMPool, withPostgresqlPool, withPostgresqlConn)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- runSqlConn :: forall backend m a. (MonadUnliftIO m, BackendCompatible SqlBackend backend) => 
--   ReaderT backend m a -> backend -> m a
--
-- withPostgresqlConn :: (MonadUnliftIO m, MonadLoggerIO m) =>
--   ConnectionString -> (SqlBackend -> m a) -> m a
--

-- createDataBase pginfo  =  runDataBaseWithLog pginfo $ runMigration migrateAll



runDataBaseWithLog :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
-- runDataBaseWithLog pginfo a = runStdoutLoggingT $ withPostgresqlConn pginfo $ \backend -> runSqlConn a backend 
runDataBaseWithLog pginfo a = runStdoutLoggingT $ withPostgresqlConn pginfo $ \backend -> runSqlConn a backend 

runDataBaseWithOutLog :: ConnectionString -> SqlPersistT (NoLoggingT IO) a -> IO a
runDataBaseWithOutLog pginfo a = runNoLoggingT $ withPostgresqlConn pginfo $ \backend -> runSqlConn a backend 

cleanUp :: (MonadIO m) => SqlPersistT m ()
cleanUp = rawExecute "TRUNCATE news, images_bank, images, categories" []

dropAll :: (MonadIO m) => SqlPersistT m ()
-- dropAll = rawExecute "DROP SCHEMA public CASCADE" []
dropAll = rawExecute "DROP TABLE news, images_bank, images, categories" []

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
 News sql=news
  title T.Text
  categoryId CategoryId
  UniqueNews title
  deriving Show
 Image sql=images
  header T.Text
  base64 T.Text
  UniqueImage header base64
  deriving Show
 ImageBank sql=images_bank
  newsId NewsId
  imageId ImageId
  UniqueImageBank newsId imageId
  deriving Show
 Category sql=categories
  label T.Text
  parent T.Text Maybe  -- to do parent CategoryId Maybe .. try to antoher table
  UniqueCategory label
  deriving Show

  -- User sql = users
  --   login T.Text
  --   name T.Text
  --   password T.Text
  --   isAdmin Bool
  --   isPublisher Bool
  --   categoryId CategoryId
  --   UniqueLabel name
  --   Primary login
  --   deriving Eq Show
  -- Category sql = categories
  --   label T.Text
  --   UniqueCategory label
  --   deriving Eq Show
  -- ProductCategory
  --   productId ProductId
  --   categoryId CategoryId
  --   Primary productId categoryId
  --   deriving Eq Show
  -- Warehouse
  --   productId ProductId
  --   quantity Int
  --   -- created UTCTime default=CURRENT_TIME
  --   -- modified UTCTime default=CURRENT_TIME
  --   deriving Eq Show
|]
