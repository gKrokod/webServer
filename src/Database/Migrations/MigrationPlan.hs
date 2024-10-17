module Database.Migrations.MigrationPlan (migrationPlan) where

import Database.Migrations.Migrationv0 (migrateVer0)
import Database.Migrations.Migrationv1 (migrateVer1)
import Database.Migrations.Migrationv2 (migrateVer2)
import Database.Migrations.Migrationv3 (migrateVer3)
import Database.Migrations.Type (MyMigration (..))

type MigrationPlan = [MyMigration]

migrationPlan :: MigrationPlan
migrationPlan =
  reverse $
    migrateVer3
      : migrateVer2
      : migrateVer1
      : migrateVer0
      : []
