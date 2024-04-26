module Base.Base where

import Base.BasicSchema (migrateAll)
import Control.Monad.Logger  (LoggingT (..), runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
-- import Data.Int (Int64)
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn)
import           Database.Persist.Sql  (SqlPersistT, runMigration)

data Config = Config {
  configConnect :: ConnectionString --BC.ByteString
}

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend
--
migrateDB :: ConnectionString -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)
--
