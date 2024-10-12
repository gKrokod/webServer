module Database.Verb (runDataBaseWithOutLog, makeAndFillTables) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (NoLoggingT (..), runNoLoggingT)
import Database.Data.FillTables (cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9, image1, image2, image3, imageBank1, imageBank2, imageBank3, imageBank4, imageBank5, news1, news2, news3, news4, password1, password2, password3, user1, user2, user3)
import Database.Esqueleto.Experimental (insertMany_)
import Database.Persist.Postgresql (ConnectionString, rawExecute, withPostgresqlConn)
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlConn)
import Scheme (migrateAll)

runDataBaseWithOutLog :: ConnectionString -> SqlPersistT (NoLoggingT IO) a -> IO a
runDataBaseWithOutLog pginfo a = runNoLoggingT $ withPostgresqlConn pginfo $ \backend -> runSqlConn a backend

makeAndFillTables :: ConnectionString -> IO ()
makeAndFillTables pginfo = makeTables pginfo >> fillTables pginfo

makeTables :: ConnectionString -> IO ()
makeTables pginfo = do
  putStrLn "Drop All tables" -- all row
  -- runDataBaseWithLog pginfo dropAll
  runDataBaseWithOutLog pginfo dropAll
  putStrLn "Make Tables in data base"
  runDataBaseWithOutLog pginfo $ runMigration migrateAll
  -- runDataBaseWithLog pginfo $ runMigration migrateAll
  pure ()

fillTables :: ConnectionString -> IO ()
fillTables pginfo = do
  putStrLn "Fill All tables"
  -- runDataBaseWithLog pginfo $ do
  runDataBaseWithOutLog pginfo $ do
    insertMany_ [image1, image2, image3]
    insertMany_ [cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9]
    insertMany_ [password1, password2, password3]
    insertMany_ [user1, user2, user3]
    insertMany_ [news1, news2, news3, news4]
    insertMany_ [imageBank1, imageBank2, imageBank3, imageBank4, imageBank5]
  pure ()

-- cleanUp :: (MonadIO m) => SqlPersistT m ()
-- cleanUp = rawExecute "TRUNCATE news, images_bank, images, categories, users, passwords" []

dropAll :: (MonadIO m) => SqlPersistT m ()
-- dropAll = rawExecute "DROP SCHEMA public CASCADE" []
dropAll = rawExecute "DROP TABLE IF EXISTS news, images_bank, images, categories, users, passwords" []
