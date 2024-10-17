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

module Database.Migrations.Migrationv1 (migrateVer1, TestMigrate (..)) where

import qualified Data.Text as T
import Database.Migrations.Type (MyMigration (..))
import Database.Persist.TH (migrateModels)
import qualified Database.Persist.TH as PTH

PTH.share
  [PTH.mkPersist PTH.sqlSettings, PTH.mkEntityDefList "addNewColumn"]
  [PTH.persistLowerCase|
 TestMigrate 
  firstColumn Int
  secondColumn T.Text
  deriving Eq Show
|]

migrateVer1 :: MyMigration
migrateVer1 = MkMigration {version = 1, description = "add Second Column", content = migrateModels addNewColumn}
