module Base.Base where
import Base.FillTables
import Scheme --(migrateAll, Category)
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlConn) 
import Control.Monad.Logger (runNoLoggingT, runStderrLoggingT, LoggingT(..), runStdoutLoggingT, NoLoggingT(..))
import Control.Monad.IO.Class (MonadIO)
import Database.Persist.Postgresql  (Entity(..), rawExecute, SqlPersistT,ConnectionString, insert, runMigration, runSqlPersistMPool, withPostgresqlPool, withPostgresqlConn)
import Database.Persist.Postgresql  (toSqlKey)
import Data.Int (Int64)
import Database.Esqueleto.Experimental (from, (^.), (==.), just, where_, table, unionAll_, val, withRecursive, select, (:&) (..), on, innerJoin , insertMany_)
-- import Database.Esqueleto.Experimental 
-- import Database.Esqueleto.Internal.Internal 
import qualified Data.Text as T
import Data.Time (UTCTime)

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
  runDataBaseWithLog pginfo $ do
  -- runDataBaseWithOutLog pginfo $ do
    insertMany_ [image1,image2,image3]
    insertMany_ [cat1,cat2, cat3,cat4,cat5,cat6,cat7,cat8, cat9]
    insertMany_ [password1,password2,password3]
    insertMany_ [user1,user2,user3]
    insertMany_ [news1, news2, news3, news4] 
    insertMany_ [imageBank1, imageBank2, imageBank3, imageBank4, imageBank5] 
  pure ()


runDataBaseWithLog :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
-- runDataBaseWithLog pginfo a = runStdoutLoggingT $ withPostgresqlConn pginfo $ \backend -> runSqlConn a backend 
runDataBaseWithLog pginfo a = runStdoutLoggingT $ withPostgresqlConn pginfo $ \backend -> runSqlConn a backend 

runDataBaseWithOutLog :: ConnectionString -> SqlPersistT (NoLoggingT IO) a -> IO a
runDataBaseWithOutLog pginfo a = runNoLoggingT $ withPostgresqlConn pginfo $ \backend -> runSqlConn a backend 

cleanUp :: (MonadIO m) => SqlPersistT m ()
cleanUp = rawExecute "TRUNCATE news, images_bank, images, categories, users, passwords" []

dropAll :: (MonadIO m) => SqlPersistT m ()
-- dropAll = rawExecute "DROP SCHEMA public CASCADE" []
dropAll = rawExecute "DROP TABLE IF EXISTS news, images_bank, images, categories, users, passwords" []

getCategories :: ConnectionString -> Int64 -> IO [Entity Category]
getCategories connString uid = runDataBaseWithLog connString fetchAction
  where
    fetchAction ::  (MonadIO m) => SqlPersistT m [Entity Category]
    fetchAction = select $ do
      cte <- withRecursive
               ( do
                   child <- from $ table @Category
                   where_ (child ^. CategoryId ==. val (toSqlKey uid))
                   pure child)
               unionAll_
               (\self -> do
                   child <- from self
                   parent <- from $ table @Category
                   where_ (just (parent ^. CategoryId) ==. child ^. CategoryParent)
                   pure parent)
      from cte

fetchImageBank :: ConnectionString -> Int64 -> IO [Entity Image]
fetchImageBank connString uid = runDataBaseWithLog connString fetchAction
  where
    fetchAction :: (MonadIO m) => SqlPersistT m [Entity Image]
    fetchAction = select $ do
      (news :& imagebank :& image) <- 
        from $ table @News
         `innerJoin` table @ImageBank
         `on`  (\(n :& i) -> n ^. NewsId ==. (i ^. ImageBankNewsId))
         `innerJoin` table @Image
         `on`  (\(_ :& i :& im) -> (i ^. ImageBankImageId) ==. (im ^. ImageId))
      where_ (news ^. NewsId ==. val (toSqlKey uid))
      pure $ (image)

type Name = T.Text
type Login = T.Text
type PasswordUser = T.Text
type ErrorDB = T.Text
type Success = Either ErrorDB T.Text

-- createUser :: (Monad m) => ConnectionString -> Name -> Login -> PasswordUser -> Bool -> Bool -> m (Int64) 
createUser :: (Monad m) => ConnectionString -> Name -> Login -> PasswordUser -> Bool -> Bool -> m (Success) 
createUser = undefined

getAllUsers :: (Monad m) => ConnectionString -> m [User]
getAllUsers = undefined

-- data Handle m = Handle 
--   {
--     logger :: Handlers.Logger.Handle m,
--     createUser :: Name -> Login -> PasswordUser -> UTCTime -> Bool -> Bool -> m (), 
--     -- createUser :: User -> m (),
--     getAllUsers :: m [User]
--     -- add some func
--   }
