{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Migrations.Migrationv5 (migrateVer5, User (..), Password (..), Unique (..), EntityField (..)) where

import qualified Data.Text as T
import Data.Time (UTCTime (..))
import Database.Migrations.Type (MyMigration (..))
import Database.Persist.Class (EntityField (..), Unique (..))
import Database.Persist.Sql.Migration (addMigration)
import Database.Persist.TH (migrateModels)
import qualified Database.Persist.TH as PTH

PTH.share
  [PTH.mkPersist PTH.sqlSettings, PTH.mkEntityDefList "addNewColumn"]
  [PTH.persistLowerCase|
 User sql=users
  name T.Text
  login T.Text
  passwordId PasswordId
  created UTCTime
  isAdmin Bool
  isPublisher Bool
  lastName T.Text Maybe
  UniqueUserLogin login
  deriving Eq Show
 Password sql=passwords
   quasiPassword T.Text
   deriving Eq Show
|]

migrateVer5 :: MyMigration
migrateVer5 = MkMigration {version = 5, description = "add new column for user", content = action}
  where
    action = do
      migrateModels addNewColumn
      addMigration False "UPDATE users SET last_name = name WHERE last_name IS NULL"
