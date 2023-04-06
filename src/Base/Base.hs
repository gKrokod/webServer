module Base.Base where

import qualified Handlers.Base
import Base.BasicSchema
import Base.TestEntity
import Control.Monad.Logger --
import Control.Monad.Reader
import Control.Monad.Reader (runReaderT)
-- -- import           Control.Monad.IO.Class (MonadIO)
import Data.Int (Int64)
import Database.Persist
import Database.Persist.Postgresql
-- import           Database.Persist.Sql
-- i

data Config = Config {
  configConnect :: ConnectionString --BC.ByteString
}

-- data Tom = Tom Int
-- data Pom
-- data Ob = Tom | Pom

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend
    -- разобраться насколько тут нужен логгинг

migrateDB :: ConnectionString -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

-- deletemigrateDB :: ConnectionString -> IO ()
-- deletemigrateDB connString = runAction connString (runMigration deleteMigrate)

createUser :: ConnectionString -> Item -> IO Int64
createUser connString (U user) = fromSqlKey <$> runAction connString (insert user)
-- createUser connString (C chel) = fromSqlKey <$> runAction connString (insert chel)
createUser connString (Ca cat) = fromSqlKey <$> runAction connString (insert cat)
createUser connString (N news) = fromSqlKey <$> runAction connString (insert news)

insertDictionary :: ConnectionString -> CategoryDictionary -> IO Int64
insertDictionary pginfo dictionary = fromSqlKey <$> runAction pginfo (insert dictionary)

fetchDictionary :: ConnectionString -> IO (Maybe CategoryDictionary)
-- после удаление содержимого таблицы, данные добавляются с другим ключом
-- и функция ничего не найдет по pK = 1
fetchDictionary pginfo = runAction pginfo $ do
  get (toSqlKey 1) 

insertCategories :: ConnectionString -> [Category] -> IO ()
insertCategories pginfo = runAction pginfo . mapM_ insert 

readUser :: ConnectionString -> Int64 -> IO (Maybe User)
readUser connString uid = runAction connString (get (toSqlKey uid))
--
deleteUserKey :: ConnectionString -> Int64 -> IO ()
deleteUserKey connString uid = runAction connString (delete userKey)
  where
    userKey :: Key User
    userKey = toSqlKey uid

deleteUser :: ConnectionString -> User -> IO ()
deleteUser connString user = runAction connString $ do
  deleteBy $ UniqueLogin (userLogin user)

insertUsers :: ConnectionString -> IO ()
insertUsers pginfo = do
  mapM_ (createUser pginfo) (map U [user1, user2, user3]  )

insertNews :: ConnectionString -> IO ()
insertNews pginfo = do
  mapM_ (createUser pginfo) (map N [news1, news2]  )

insertAll :: ConnectionString -> IO ()
insertAll pginfo = do
  mapM_ (createUser pginfo) (map U [user1, user2, user3]  )
  -- mapM_ (createUser pginfo) (map C [chel1, chel2, chel3]  )
  -- mapM_ (createUser pginfo) (map Ca [cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9]  )
  mapM_ (createUser pginfo) (map N [news1, news2]  )
  -- runAction pginfo $ do
  --   a <- insert catTr1
  --   pure ()

deleteAll :: ConnectionString -> IO ()
deleteAll pginfo = runAction pginfo $ do 
  -- deleteWhere ([] :: [Filter Chel])
  deleteWhere ([] :: [Filter News])
  deleteWhere ([] :: [Filter User])
  deleteWhere ([] :: [Filter Category])
  deleteWhere ([] :: [Filter CategoryDictionary])

insertCat :: ConnectionString -> IO ()
insertCat pginfo = createUser pginfo (Ca cat1) >> pure ()

deleteUsers :: ConnectionString -> IO ()
deleteUsers pginfo = do
  mapM_ (deleteUser pginfo) [user1, user2, user3]  
  
fetchUser :: ConnectionString -> IO (Maybe User)
fetchUser pginfo = runAction pginfo $ do
  -- getBy $ UniqueName (categoryName ct)
  get (toSqlKey 1) 

fetchCat :: ConnectionString -> IO (Maybe Category)
fetchCat pginfo = runAction pginfo $ do
  get (toSqlKey 1) 

-- fetchTree :: ConnectionString -> IO (Maybe CategoryDictionary)
-- fetchTree pginfo = runAction pginfo $ do
--   get (toSqlKey 1) 

fetchNews :: ConnectionString -> IO (Maybe News)
fetchNews pginfo = runAction pginfo $ do
  get (toSqlKey 1) 
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
