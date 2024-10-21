module Database.Migrations.Migrationv3 (migrateVer3) where

import Database.Migrations.Type (MyMigration (..))
import Database.Persist.Sql (rawExecute)
import Database.Persist.Sql.Migration (runSqlCommand)

migrateVer3 :: MyMigration
migrateVer3 = MkMigration {version = 3, description = "delete first column", content = deleteFirstColumn}
  where
    deleteFirstColumn = do
      runSqlCommand $
        rawExecute "ALTER TABLE test_migrate DROP COLUMN first_column" []
