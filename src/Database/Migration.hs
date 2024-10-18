module Database.Migration (migratePlan, MyMigration(..), MigrateTable(..)) where

import Database.Migrations.Type (MyMigration(..), MigrateTable(..))
import Database.Migrations.Migrationv0 (migrateVer0)

type MigratePlan = [MyMigration]

migratePlan :: MigratePlan
migratePlan = migrateVer0 : []

----


-- migration :: Migration
-- migration = do
--     addMigration "CREATE EXTENSION IF NOT EXISTS "citext";"
--     migrateAll
--
--
--addMigrations :: CautiousMigration -> Migration
--
--migrateModels :: [EntityDef] -> Migration
--
--
--migrate :: [EntityDef] -> EntityDef -> Migration
--
--

