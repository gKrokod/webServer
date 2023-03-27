module Base.Base where

import qualified Handlers.Base
import Base.BasicSchema
import           Control.Monad.Logger --
import           Control.Monad.Reader (runReaderT)
-- -- import           Control.Monad.IO.Class (MonadIO)
import           Data.Int (Int64)
import           Database.Persist
import           Database.Persist.Postgresql
-- import           Database.Persist.Sql

data Config = Config {
  configConnect :: ConnectionString --BC.ByteString
}

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend
    -- разобраться насколько тут нужен логгинг

migrateDB :: ConnectionString -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)
{--
createUser :: ConnectionString -> User -> IO Int64
createUser connString user = fromSqlKey <$> runAction connString (insert user)

readUser :: ConnectionString -> Int64 -> IO (Maybe User)
readUser connString uid = runAction connString (get (toSqlKey uid))

deleteUser :: ConnectionString -> Int64 -> IO ()
deleteUser connString uid = runAction connString (delete userKey)
  where
    userKey :: Key User
    userKey = toSqlKey uid
    --}
{--
-- selectYoungTeachers' :: (MonadIO m) => SqlPersistT m [Entity User]
-- selectYoungTeachers' = selectList
--   [UserAge <. 25, UserOccupation ==. "Teacher"] [Asc UserEmail, OffsetBy 5, LimitTo 100]

  
-- insertUser :: Config -> User -> IO ()
-- insertUser cfg user = runAction (configConnect cfg) $ do
--   insert user
--   liftIO $ do {print ( "insert\n"); print user}
--
-- deleteUser :: Config -> User -> IO ()
-- deleteUser cfg user = runAction (configConnect cfg) $ do
--   deleteBy $ UniqueEmail (userEmail user)
--   liftIO $ do {print ( "delete\n"); print user}
--
-- updateUser :: ConfigDB -> User -> IO ()
-- updateUser cfg user = runAction connectionCfg $ do
--   -- pid <- insert user
--   -- pid <- selectFirst $ UniqueEmail (userEmail user)
--   pid <- selectKeys [UserAge <. 20] []
--   -- update pid [UserAge =. 5]
--   liftIO $ do {print ( "insert\n"); print user}

type Email = Text


-- printUser :: Config -> Email -> IO ()
-- printUser cfg email = runAction (configConnect cfg) $ do
--   r <- getBy $ UniqueEmail email
--   -- r <- selectList [UserAge <. 30] []
--   liftIO $ print $ r 
    --}
