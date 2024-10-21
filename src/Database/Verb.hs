module Database.Verb (insertTestData, runDataBaseWithOutLog) where

import Control.Exception (SomeException, try)
import Control.Monad.Logger (NoLoggingT (..), runNoLoggingT)
import Database.Data.FillTables (cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9, image1, image2, image3, imageBank1, imageBank2, imageBank3, imageBank4, imageBank5, news1, news2, news3, news4, password1, password2, password3, user1, user2, user3)
import Database.Esqueleto.Experimental (insertMany_)
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn)
import Database.Persist.Sql (SqlPersistT, runSqlConn)
import Handlers.Database.Base (Success (..))

runDataBaseWithOutLog :: ConnectionString -> SqlPersistT (NoLoggingT IO) a -> IO a
runDataBaseWithOutLog pginfo a = runNoLoggingT $ withPostgresqlConn pginfo $ \backend -> runSqlConn a backend

insertTestData :: ConnectionString -> IO (Either SomeException Success)
insertTestData pginfo = do
  try @SomeException
    ( runDataBaseWithOutLog pginfo $ do
        insertMany_ [image1, image2, image3]
        insertMany_ [cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9]
        insertMany_ [password1, password2, password3]
        insertMany_ [user1, user2, user3]
        insertMany_ [news1, news2, news3, news4]
        insertMany_ [imageBank1, imageBank2, imageBank3, imageBank4, imageBank5]
        pure Put
    )
