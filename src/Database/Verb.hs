module Database.Verb (runDataBaseWithOutLog) where

import Control.Monad.Logger (NoLoggingT (..), runNoLoggingT)
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn)
import Database.Persist.Sql (SqlPersistT, runSqlConn)

runDataBaseWithOutLog :: ConnectionString -> SqlPersistT (NoLoggingT IO) a -> IO a
runDataBaseWithOutLog pginfo a = runNoLoggingT $ withPostgresqlConn pginfo $ \backend -> runSqlConn a backend
