module Database.Migrations.Migrationv4 (migrateVer4) where

import Database.Data.FillTables (cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9, image1, image2, image3, imageBank1, imageBank2, imageBank3, imageBank4, imageBank5, news1, news2, news3, news4, password1, password2, password3, user1, user2, user3)
import Database.Esqueleto.Experimental (insertMany_)
import Database.Migrations.Type (MyMigration (..))
import Database.Persist.Sql.Migration (runSqlCommand)

migrateVer4 :: MyMigration
migrateVer4 = MkMigration {version = 4, description = "add test data", content = action}
  where
    action = runSqlCommand $ do
      insertMany_ [image1, image2, image3]
      insertMany_ [cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9]
      insertMany_ [password1, password2, password3]
      insertMany_ [user1, user2, user3]
      insertMany_ [news1, news2, news3, news4]
      insertMany_ [imageBank1, imageBank2, imageBank3, imageBank4, imageBank5]
