module Base.Base where
import Base.FillTables
import Scheme --(migrateAll, Category)
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlConn) 
import Control.Monad.Logger (runNoLoggingT, runStderrLoggingT, LoggingT(..), runStdoutLoggingT, NoLoggingT(..))
import Control.Monad.IO.Class (MonadIO)
import Database.Persist.Postgresql  (Entity(..), rawExecute, SqlPersistT,ConnectionString, runMigration, runSqlPersistMPool, withPostgresqlPool, withPostgresqlConn)
import Database.Persist.Postgresql  (toSqlKey)
import Data.Int (Int64)
import Database.Esqueleto.Experimental (from, (^.), (==.), just, where_, table, unionAll_, val, withRecursive, select, (:&) (..), on, innerJoin , insertMany_, insertMany)
import Database.Esqueleto.Experimental (getBy, limit, insert, insert_, replace, get, fromSqlKey, delete, selectOne, valList, in_, Value(..))
import qualified Data.Text as T
import Data.Time (UTCTime)
import Handlers.Base (Success(..), Name, Login, PasswordUser, Label, NewLabel, Header, Base64,  Content, Title, NewsOut)
import Handlers.Base (NumberImage, URI_Image)
import Data.Time (getCurrentTime)
import Control.Exception (try, SomeException, throwIO, Exception, throw)
--
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


pullAllNews :: ConnectionString -> LimitData -> IO (Either SomeException [NewsOut])
-- getAllNews connString l = runDataBaseWithLog connString fetchAction
pullAllNews connString l = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
    where 
      fetchAction :: (MonadFail m, MonadIO m) => SqlPersistT m [NewsOut]
      fetchAction = do
        titles <- (fmap . fmap) unValue
                  (select $ do
                     news <- from $ table @News
                     limit (fromIntegral l)
                     pure (news ^. NewsTitle))
        mapM (fetchFullNews l) titles 

    
fetchLables :: (MonadIO m) => LimitData -> Label -> SqlPersistT m [Entity Category]
-- todo выяснить, почему трубется Database.Esqueleto.Internal.Internal.SqlExpr (Value Title)
fetchLables lim label = 
      select $ do
      cte <- withRecursive
               ( do
                   child <- from $ table @Category
                   where_ (child ^. CategoryLabel ==. val label)
                   pure child)
               unionAll_
               (\self -> do
                   child <- from self
                   parent <- from $ table @Category
                   where_ (just (parent ^. CategoryId) ==. child ^. CategoryParent)
                   pure parent)
      limit (fromIntegral lim)
      from cte

fetchFullNews :: (MonadFail m, MonadIO m) => LimitData -> Title -> SqlPersistT m NewsOut
fetchFullNews l title = do
  (label : _) <- (fmap . fmap) entityVal fetchLabel
  lables <- fetchLables (categoryLabel label)
  (user : _) <- (fmap . fmap) entityVal fetchUser
  images <- fetchActionImage
  (Just partNews) <- (fmap . fmap) entityVal (getBy $ UniqueNews title)
  let a = (title,
            newsCreated partNews,
            userName user, 
            workerCategory lables, 
            newsContent partNews,
            workerImage images,
            newsIsPublish partNews)
  pure a 
    where
      fetchLabel :: (MonadIO m) => SqlPersistT m [Entity Category]
      fetchLabel = select $ do
        (news :& category) <- 
          from $ table @News
           `innerJoin` table @Category
           `on`  (\(n :& c) -> n ^. NewsCategoryId ==. (c ^. CategoryId))
        where_ (news ^. NewsTitle ==. (val title))
        limit (fromIntegral l)
        pure (category)

      fetchUser :: (MonadIO m) => SqlPersistT m [Entity User]
      fetchUser = select $ do
        (news :& user) <- 
          from $ table @News
           `innerJoin` table @User
           `on`  (\(n :& c) -> n ^. NewsUserId ==. (c ^. UserId))
        where_ (news ^. NewsTitle ==. (val title))
        limit (fromIntegral l)
        pure user

      fetchLables :: (MonadIO m) => Label -> SqlPersistT m [Entity Category]
      fetchLables label = 
            select $ do
            cte <- withRecursive
                     ( do
                         child <- from $ table @Category
                         where_ (child ^. CategoryLabel ==. val label)
                         pure child)
                     unionAll_
                     (\self -> do
                         child <- from self
                         parent <- from $ table @Category
                         where_ (just (parent ^. CategoryId) ==. child ^. CategoryParent)
                         pure parent)
            limit (fromIntegral l)
            from cte

      fetchActionImage :: (MonadIO m) => SqlPersistT m [Entity Image]
      fetchActionImage = select $ do
        (news :& imagebank :& image) <- 
          from $ table @News
           `innerJoin` table @ImageBank
           `on`  (\(n :& i) -> n ^. NewsId ==. (i ^. ImageBankNewsId))
           `innerJoin` table @Image
           `on`  (\(_ :& i :& im) -> (i ^. ImageBankImageId) ==. (im ^. ImageId))
        where_ (news ^. NewsTitle ==. (val title))
        limit (fromIntegral l)
        pure (image)

      workerImage :: [Entity Image] -> [URI_Image]
      workerImage = map (\(Entity key _value) -> ("/images?id=" <> (T.pack $ show $ fromSqlKey key)))

      workerCategory :: [Entity Category] -> [Label]
      workerCategory = map (\(Entity _key value) -> categoryLabel value) 

findNewsByTitle :: ConnectionString -> Title -> IO (Maybe News) 
-- findUserByLogin connString login = runDataBaseWithLog connString fetchAction
findNewsByTitle connString title = runDataBaseWithOutLog connString fetchAction
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe News)
    fetchAction = (fmap . fmap) entityVal (getBy $ UniqueNews title)

editNews :: ConnectionString -> Title -> UTCTime -> Maybe Title -> Maybe Login -> Maybe Label -> Maybe Content -> [Image] -> Maybe Bool -> IO (Either SomeException Success)
editNews pginfo title time newTitle  newLogin newLabel newContent newImages newPublish = 
  try @SomeException (runDataBaseWithOutLog pginfo $ do
    oldNews <- getBy $ UniqueNews title
    case oldNews of
      Just oldNews' -> do
        let (News oldTitle oldTime oldKeyUser oldKeyCategory oldContent oldPublish) = entityVal oldNews' 
        let keyNews = entityKey oldNews' 
        newKeyUser <- case newLogin of
                     Just login -> (fmap . fmap) entityKey (getBy $ UniqueUserLogin login)
                     _ -> pure Nothing
        newKeyCategory <- case newLabel of
                     Just label -> (fmap . fmap) entityKey (getBy $ UniqueCategoryLabel label)
                     _ -> pure Nothing
        let newNews = News (replaceField oldTitle newTitle) 
                           time 
                           (replaceField oldKeyUser newKeyUser) 
                           (replaceField oldKeyCategory newKeyCategory) 
                           (replaceField oldContent newContent) 
                           (replaceField oldPublish newPublish)
        replace keyNews newNews
        deleteImagesFromBankByNews keyNews
        keysImages <- insertMany newImages
        insertMany_ $ zipWith ImageBank (cycle [keyNews]) keysImages
        pure Change
      Nothing -> throw $ userError "function editNews fail (can't find news)")
deleteImagesFromBankByNews :: (MonadIO m) => Key News -> SqlPersistT m ()
deleteImagesFromBankByNews key =  
      delete $ do
        imageBank <- from $ table @ImageBank
        where_ (imageBank ^. ImageBankNewsId ==. val key)

putNews :: ConnectionString -> Title -> UTCTime -> Login -> Label -> Content -> [Image] -> Bool -> IO (Either SomeException Success) 
putNews pginfo title time login label content images ispublish = 
  try @SomeException (runDataBaseWithOutLog pginfo $ do
    keyUser <- (fmap . fmap) entityKey (getBy $ UniqueUserLogin login)
    keyCategory <- (fmap . fmap) entityKey (getBy $ UniqueCategoryLabel label)
    case (keyUser, keyCategory) of
      (Just keyUsr, Just keyCat) -> do
        keyNews <- insert $ News title time keyUsr keyCat content ispublish 
        keysImages <- insertMany images 
        insertMany_ $ zipWith ImageBank (cycle [keyNews]) keysImages
        pure Put
      _ -> throw $ userError "function putNews fail")

------------------------------------------------------------------------------------------------------------

  --for api news
pullImage :: ConnectionString -> NumberImage -> IO (Either SomeException (Maybe Image)) 
-- getImage :: ConnectionString -> Int64 -> IO (Maybe Image) 
pullImage connString uid = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe Image)
    fetchAction = get (toSqlKey uid)

-- deleteImagesFromBank :: ConnectionString -> Title -> IO () 
-- deleteImagesFromBank connString title = runDataBaseWithOutLog connString fetchAction
-- -- deleteImagesFromBank connString title = runDataBaseWithLog connString fetchAction
--   where
--     fetchAction :: (MonadIO m) => SqlPersistT m ()
--     fetchAction = do 
--       keyNews <- (fmap . fmap) entityKey (getBy $ UniqueNews title)
--       case keyNews of
--         Just keyNews' -> 
--           delete $ do
--             imageBank <- from $ table @ImageBank
--             where_ (imageBank ^. ImageBankNewsId ==. val keyNews')
--         _ -> pure ()    

------------------------------------------------------------------------------------------------------------
putUser :: ConnectionString -> Name -> Login -> PasswordUser -> UTCTime -> Bool -> Bool -> IO (Either SomeException Success) 
-- putUser :: ConnectionString -> Name -> Login -> PasswordUser -> UTCTime -> Bool -> Bool -> IO () 
putUser pginfo name login pwd time admin publish  = do
  try @SomeException (runDataBaseWithOutLog pginfo $ do
  -- runDataBaseWithLog pginfo $ do
    pId <- insert $ Password pwd 
    insert $ User name login pId time admin publish
    pure Put)

findUserByLogin :: ConnectionString -> Login -> IO (Maybe User) 
-- findUserByLogin connString login = runDataBaseWithLog connString fetchAction
findUserByLogin connString login = runDataBaseWithOutLog connString fetchAction
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe User)
    fetchAction = (fmap . fmap) entityVal (getBy $ UniqueUserLogin login)

type LimitData = Int

pullAllUsers :: ConnectionString -> LimitData -> IO (Either SomeException [User])
pullAllUsers connString l = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
  -- e <- try @SomeException (runDataBaseWithOutLog connString fetchAction)
  -- pure $ either (\x -> []) (id) e
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
 
putCategory :: ConnectionString -> Label -> Maybe Label -> IO (Either SomeException Success) 
putCategory pginfo label parent = 
  try @SomeException (runDataBaseWithOutLog pginfo $ do
  -- runDataBaseWithLog pginfo $ do
    case parent of
      Nothing -> do
        insert_ $ Category label Nothing  
        pure Put
      Just labelParent -> do
        parentId <- (fmap . fmap) entityKey <$> getBy $ UniqueCategoryLabel labelParent 
        insert_ $ Category label parentId
        pure Put)
--
editCategory :: ConnectionString -> Label -> NewLabel -> Maybe Label -> IO (Either SomeException Success) 
editCategory pginfo label newLabel parent = do
  try @SomeException (runDataBaseWithOutLog pginfo $ do
  -- runDataBaseWithLog pginfo $ do
    labelId <- (fmap . fmap) entityKey <$> getBy $ UniqueCategoryLabel label 
    case (labelId, parent) of
      (Just iD, Nothing) -> replace iD $ Category newLabel Nothing
      (Just iD , Just labelParent) -> do
        parentId <- (fmap . fmap) entityKey <$> getBy $ UniqueCategoryLabel labelParent
        replace iD $ Category newLabel parentId
      _ -> throw $ userError "function editCategory fail (can't find category)"  -- label don't exist
    pure Change)

findCategoryByLabel :: ConnectionString -> Label -> IO (Maybe Category) 
findCategoryByLabel connString label = runDataBaseWithOutLog connString fetchAction
-- findCategoryByLabel connString label = runDataBaseWithLog connString fetchAction
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe Category)
    fetchAction = (fmap . fmap) entityVal (getBy $ UniqueCategoryLabel label)


pullAllCategories :: ConnectionString -> LimitData -> IO (Either SomeException [Category])
-- pullAllCategories connString l = runDataBaseWithLog connString fetchAction
pullAllCategories connString l = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
    where
      fetchAction ::  (MonadIO m) => SqlPersistT m [Category]
      fetchAction = (fmap . fmap) entityVal 
                    (select $ do
                    categories <- from $ table @Category
                    limit (fromIntegral l)
                    pure (categories))


replaceField :: a -> Maybe a -> a
replaceField _ (Just a) = a
replaceField a _ = a
------------------------------------------------------------------------------------------------------------
