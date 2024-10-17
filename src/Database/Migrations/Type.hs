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
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

module Database.Migrations.Type (MyMigration (..), MigrateTable (..), createMigrateTable, EntityField (..)) where

import Data.Text (Text, unpack)
import Data.Time (UTCTime (..))
import Database.Persist.Sql (EntityField, Migration)
import qualified Database.Persist.TH as PTH

data MyMigration = MkMigration
  { version :: Int,
    description :: Text,
    content :: Migration
  }

instance Show MyMigration where
  show (MkMigration v d _) = show v <> ": " <> unpack d

PTH.share
  [PTH.mkPersist PTH.sqlSettings, PTH.mkEntityDefList "createMigrateTable"]
  [PTH.persistLowerCase|
 MigrateTable sql=migrate_table
  version Int
  description Text
  time UTCTime
  UniqueMigrateTableVersion version
  deriving Eq Show
 TestMigrate 
  firstColumn Int
  deriving Eq Show
|]
