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

module Database.Migrations.Migrationv5 (migrateVer5, User (..), Password (..), Category (..), News (..), Image (..), ImageBank (..), Unique (..), EntityField (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Text as T
import Data.Time (UTCTime (..))
import Database.Migrations.Type (MyMigration (..))
import Database.Persist.Class (EntityField (..), Unique (..))
import Database.Persist.Sql.Migration (addMigration)
import Database.Persist.TH (migrateModels)
import qualified Database.Persist.TH as PTH
import GHC.Generics (Generic)

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
 Category sql=categories
  label T.Text
  parent CategoryId Maybe
  UniqueCategoryLabel label
  deriving Eq Show 
 News sql=news
  title T.Text
  created UTCTime
  userId UserId
  categoryId CategoryId
  content T.Text
  isPublish Bool
  UniqueNews title
  deriving Eq Show
 Image sql=images
  header T.Text
  base64 T.Text
  deriving Eq Show Generic FromJSON ToJSON
 ImageBank sql=images_bank
  newsId NewsId
  imageId ImageId
  Primary newsId imageId
  deriving Eq Show
|]

migrateVer5 :: MyMigration
migrateVer5 = MkMigration {version = 5, description = "add new column for user", content = action}
  where
    action = do
      migrateModels addNewColumn
      addMigration False "UPDATE users SET last_name = name WHERE last_name IS NULL"
