module Database.Migrations.Migrationv2 (migrateVer2) where

import Database.Migrations.Type (MyMigration (..))
import Database.Persist.PersistValue (PersistValue (..))
import Database.Persist.Sql (rawExecute)
import Database.Persist.Sql.Migration (runSqlCommand)

migrateVer2 :: MyMigration
migrateVer2 = MkMigration {version = 2, description = "insert data ", content = insertData}
  where
    insertData = do
      runSqlCommand $
        rawExecute "INSERT INTO test_migrate (first_column, second_column) VALUES (?, ?)" [PersistInt64 111, PersistText "zayac"]
