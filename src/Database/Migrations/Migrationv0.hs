module Database.Migrations.Migrationv0 (migrateVer0) where

import Database.Migrations.Type (MyMigration(..), createMigrateTable)
import Schema (createTablesForEntity)
import Database.Persist.TH (migrateModels)

migrateVer0 :: MyMigration
migrateVer0 = MkMigration {version = 0, description = "create all tables", content = migrateModels (createMigrateTable <> createTablesForEntity)}
 
