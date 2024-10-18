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

module Database.Migrations.Type (MyMigration(..), MigrateTable(..), createMigrateTable) where

import Data.Text (Text)
import Database.Persist.Sql (Migration)
import qualified Database.Persist.TH as PTH
import Data.Time (UTCTime (..))

data MyMigration = MkMigration {
 version :: Int,
 description :: Text,
 content :: Migration
                                  } 

PTH.share
  [PTH.mkPersist PTH.sqlSettings, PTH.mkEntityDefList "createMigrateTable"]
  [PTH.persistLowerCase|
 MigrateTable sql=migrate_table
  version Int
  description Text
  time UTCTime
  UniqueMigrateTableVersion version
  deriving Eq Show
|]

