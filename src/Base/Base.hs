module Base.Base where
import Base.FillTables
import Scheme --(migrateAll, Category)
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlConn) 
import Control.Monad.Logger (runNoLoggingT, runStderrLoggingT, LoggingT(..), runStdoutLoggingT, NoLoggingT(..))
import Control.Monad.IO.Class (MonadIO)
import Database.Persist.Postgresql  (Entity(..), rawExecute, SqlPersistT,ConnectionString, runMigration, runSqlPersistMPool, withPostgresqlPool, withPostgresqlConn)
-- import Database.Persist.Postgresql  (getBy)
import Database.Persist.Postgresql  (toSqlKey)
import Data.Int (Int64)
import Database.Esqueleto.Experimental (from, (^.), (==.), just, where_, table, unionAll_, val, withRecursive, select, (:&) (..), on, innerJoin , insertMany_, insertMany)
import Database.Esqueleto.Experimental (getBy, limit, insert, replace, get, fromSqlKey)
-- import Database.Esqueleto.Experimental 
-- import Database.Esqueleto.Internal.Internal 
import qualified Data.Text as T
import Data.Time (UTCTime)
import Handlers.Base (Success(..), Name, Login, PasswordUser, Label, NewLabel, Header, Base64, NumberImage, Content, Title)
import Handlers.Base (KeyIdUser, KeyIdCategory)
import Data.Time (getCurrentTime)
import Control.Exception (throwIO)

-- type Name = T.Text
-- type Login = T.Text
-- type PasswordUser = T.Text
-- type Header = T.Text
-- type Base64 = T.Text
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

    -- getImage :: NumberImage -> m (Maybe Image),
    -- putImage :: Header -> Base64 -> m () -- todo
    --
    
getAllNews :: ConnectionString -> LimitData -> IO [News]
getAllNews connString l = runDataBaseWithOutLog connString fetchAction
-- getAllUsers connString l = runDataBaseWithLog connString fetchAction
  where
    fetchAction ::  (MonadIO m) => SqlPersistT m [News]
    fetchAction = (fmap . fmap) entityVal 
                  (select $ do
                  news <- from $ table @News
                  limit (fromIntegral l)
                  pure (news))

getNews' :: ConnectionString -> Int64 -> IO (Maybe News)
getNews' connString uid = runDataBaseWithLog connString (getNews uid) 

getNews :: MonadIO m => Int64 -> SqlPersistT m (Maybe News)
getNews n = get (toSqlKey n) 

getUser :: MonadIO m => Int64 -> SqlPersistT m (Maybe User)
getUser n = get (toSqlKey n) 

allNews :: MonadIO m => SqlPersistT m [Entity News]
allNews = select $ do
  news <- from $ table @News
  pure news

getFullNews :: ConnectionString -> Int64 -> IO (Maybe News, Maybe User, [Entity Category], [Entity Image])
getFullNews connString uid = runDataBaseWithLog connString $ do
  news <- getNews uid
  let userid = maybe 0 (fromSqlKey . newsUserId) news 
  let catid = maybe 0 (fromSqlKey . newsCategoryId) news 
  user <- getUser userid 
  cat <- fetchActionCat catid
  images <- fetchActionImage uid
  pure (news, user, cat, images)


fetchActionImage :: (MonadIO m) => Int64 -> SqlPersistT m [Entity Image]
fetchActionImage nuid = select $ do
  (news :& imagebank :& image) <- 
    from $ table @News
     `innerJoin` table @ImageBank
     `on`  (\(n :& i) -> n ^. NewsId ==. (i ^. ImageBankNewsId))
     `innerJoin` table @Image
     `on`  (\(_ :& i :& im) -> (i ^. ImageBankImageId) ==. (im ^. ImageId))
  where_ (news ^. NewsId ==. val (toSqlKey nuid))
  pure $ (image)

fetchActionCat ::  (MonadIO m) => Int64 -> SqlPersistT m [Entity Category]
fetchActionCat uid = select $ do
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

fetchNewsUser :: ConnectionString -> Int64 -> IO [Entity News]
fetchNewsUser connString uid = runDataBaseWithLog connString fetchAction
  where
    fetchAction :: (MonadIO m) => SqlPersistT m [Entity News]
    fetchAction = select $ do
      news <- from $ table @News
      where_ (news ^. NewsUserId ==. val (toSqlKey uid))
      pure news


-- putNews :: ConnectionString -> Title -> UTCTime -> KeyIdUser -> KeyIdCategory -> Content -> [Image] -> Bool -> IO () 
-- -- putNews :: ConnectionString -> Title -> UTCTime -> Int64 -> Int64 -> Content -> [Image] -> Bool -> IO () 
-- -- тип надо вот таким сделать, чтобы не проверять наличие юзера и категории. А эту проверку вынести наверх.
-- putNews pginfo title time keyUser keyCategory content images ispublish = 
--   runDataBaseWithOutLog pginfo $ do
--     keyNews <- insert $ News title time (toSqlKey keyUser) (toSqlKey keyCategory) content ispublish 
--     keysImages <- insertMany images 
--     insertMany_ $ zipWith ImageBank (cycle [keyNews]) keysImages

findNewsByTitle :: ConnectionString -> Title -> IO (Maybe News) 
-- findUserByLogin connString login = runDataBaseWithLog connString fetchAction
findNewsByTitle connString title = runDataBaseWithOutLog connString fetchAction
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe News)
    fetchAction = (fmap . fmap) entityVal (getBy $ UniqueNews title)

putNews :: ConnectionString -> Title -> UTCTime -> Login -> Label -> Content -> [Image] -> Bool -> IO () 
putNews pginfo title time login label content images ispublish = 
  runDataBaseWithOutLog pginfo $ do
    -- keyUser <- maybe (throwIO "User didn't find") id <$> (fmap . fmap) entityKey (getBy $ UniqueUserLogin login)
    keyUser <- (fmap . fmap) entityKey (getBy $ UniqueUserLogin login)
    -- keyCategory <- maybe (throwIO "Category didn't find") id <$> (fmap . fmap) entityKey (getBy $ UniqueCategoryLabel label)
    keyCategory <- (fmap . fmap) entityKey (getBy $ UniqueCategoryLabel label)
    case (keyUser, keyCategory) of
      (Just ku, Just kc) -> do
        keyNews <- insert $ News title time ku kc content ispublish 
        keysImages <- insertMany images 
        insertMany_ $ zipWith ImageBank (cycle [keyNews]) keysImages
      _ -> do
        throwTo (userError "User or Label didnt' find") 
        pure ()

------------------------------------------------------------------------------------------------------------
putImage :: ConnectionString -> Header -> Base64 -> IO () 
putImage pginfo header base64 = do
  runDataBaseWithOutLog pginfo $ do
  -- runDataBaseWithLog pginfo $ do
    insert $ Image header base64
    pure ()

  --for api news
getImage :: ConnectionString -> NumberImage -> IO (Maybe Image) 
-- getImage :: ConnectionString -> Int64 -> IO (Maybe Image) 
getImage connString uid = runDataBaseWithOutLog connString fetchAction
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe Image)
    fetchAction = get (toSqlKey uid)

-- ishet vse kartinki novosti
-- fetchImageBank :: ConnectionString -> Int64 -> IO [Entity Image]
-- fetchImageBank connString uid = runDataBaseWithOutLog connString fetchAction
-- -- fetchImageBank connString uid = runDataBaseWithLog connString fetchAction
--   where
--     fetchAction :: (MonadIO m) => SqlPersistT m [Entity Image]
--     fetchAction = select $ do
--       (news :& imagebank :& image) <- 
--         from $ table @News
--          `innerJoin` table @ImageBank
--          `on`  (\(n :& i) -> n ^. NewsId ==. (i ^. ImageBankNewsId))
--          `innerJoin` table @Image
--          `on`  (\(_ :& i :& im) -> (i ^. ImageBankImageId) ==. (im ^. ImageId))
--       where_ (news ^. NewsId ==. val (toSqlKey uid))
--       pure $ (image)
------------------------------------------------------------------------------------------------------------
putUser :: ConnectionString -> Name -> Login -> PasswordUser -> UTCTime -> Bool -> Bool -> IO () 
putUser pginfo name login pwd time admin publish  = do
  runDataBaseWithOutLog pginfo $ do
  -- runDataBaseWithLog pginfo $ do
    pId <- insert $ Password pwd 
    insert $ User name login pId time admin publish
    pure ()

findUserByLogin :: ConnectionString -> Login -> IO (Maybe User) 
-- findUserByLogin connString login = runDataBaseWithLog connString fetchAction
findUserByLogin connString login = runDataBaseWithOutLog connString fetchAction
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe User)
    fetchAction = (fmap . fmap) entityVal (getBy $ UniqueUserLogin login)

type LimitData = Int

getAllUsers :: ConnectionString -> LimitData -> IO [User]
getAllUsers connString l = runDataBaseWithOutLog connString fetchAction
-- getAllUsers connString l = runDataBaseWithLog connString fetchAction
  where
    -- fetchAction ::  (MonadIO m) => SqlPersistT m [Entity User]
    fetchAction ::  (MonadIO m) => SqlPersistT m [User]
    fetchAction = (fmap . fmap) entityVal 
                  (select $ do
                  users <- from $ table @User
                  limit (fromIntegral l)
                  pure (users))
------------------------------------------------------------------------------------------------------------
 
putCategory :: ConnectionString -> Label -> Maybe Label -> IO () 
putCategory pginfo label parent = do
  runDataBaseWithOutLog pginfo $ do
  -- runDataBaseWithLog pginfo $ do
    case parent of
      Nothing -> insert $ Category label Nothing
      Just labelParent -> do
        parentId <- (fmap . fmap) entityKey <$> getBy $ UniqueCategoryLabel labelParent 
        -- esli nothing to yze opisano v handlers, ne vuzuvaetsya
        insert $ Category label parentId 
    pure ()

changeCategory :: ConnectionString -> Label -> NewLabel -> Maybe Label -> IO () 
changeCategory pginfo label newLabel parent = do
  runDataBaseWithOutLog pginfo $ do
  -- runDataBaseWithLog pginfo $ do
    labelId <- (fmap . fmap) entityKey <$> getBy $ UniqueCategoryLabel label 
    case (labelId, parent) of
      (Just id, Nothing) -> replace id $ Category newLabel Nothing
      (Just id , Just labelParent) -> do
        parentId <- (fmap . fmap) entityKey <$> getBy $ UniqueCategoryLabel labelParent
        replace id $ Category newLabel parentId
      _ -> pure ()  -- label don't exist
    pure ()

findCategoryByLabel :: ConnectionString -> Label -> IO (Maybe Category) 
findCategoryByLabel connString label = runDataBaseWithOutLog connString fetchAction
-- findCategoryByLabel connString label = runDataBaseWithLog connString fetchAction
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe Category)
    fetchAction = (fmap . fmap) entityVal (getBy $ UniqueCategoryLabel label)


getAllCategories :: ConnectionString -> LimitData -> IO [Category]
-- getAllCategories connString l = runDataBaseWithLog connString fetchAction
getAllCategories connString l = runDataBaseWithOutLog connString fetchAction
  where
    -- fetchAction ::  (MonadIO m) => SqlPersistT m [Entity User]
    fetchAction ::  (MonadIO m) => SqlPersistT m [Category]
    fetchAction = (fmap . fmap) entityVal 
                  (select $ do
                  categories <- from $ table @Category
                  limit (fromIntegral l)
                  pure (categories))

getBranchCategories :: ConnectionString -> LimitData -> Label -> IO [Category]
getBranchCategories connString l label = runDataBaseWithOutLog connString fetchAction 
  where
    fetchAction ::  (MonadIO m) => SqlPersistT m [Category]
    fetchAction = (fmap . fmap) entityVal 
      (select $ do
      cte <- withRecursive
               ( do
                   child <- from $ table @Category
                   where_ (child ^. CategoryLabel ==. val (label))
                   pure child)
               unionAll_
               (\self -> do
                   child <- from self
                   parent <- from $ table @Category
                   where_ (just (parent ^. CategoryId) ==. child ^. CategoryParent)
                   pure parent)
      limit (fromIntegral l)
      from cte)

-- getCategories :: ConnectionString -> Int64 -> IO [Entity Category] --for debugging
-- -- getCategories connString uid = runDataBaseWithOutLog connString fetchAction
-- getCategories connString uid = runDataBaseWithLog connString fetchAction
--   where
--     fetchAction ::  (MonadIO m) => SqlPersistT m [Entity Category]
--     fetchAction = select $ do
--       cte <- withRecursive
--                ( do
--                    child <- from $ table @Category
--                    where_ (child ^. CategoryId ==. val (toSqlKey uid))
--                    pure child)
--                unionAll_
--                (\self -> do
--                    child <- from self
--                    parent <- from $ table @Category
--                    where_ (just (parent ^. CategoryId) ==. child ^. CategoryParent)
--                    pure parent)
--       -- limit 1
--       from cte

------------------------------------------------------------------------------------------------------------
