module Base.Base where

import qualified Base.Crypto
import Base.FillTables (cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9, image1, image2, image3, imageBank1, imageBank2, imageBank3, imageBank4, imageBank5, news1, news2, news3, news4, password1, password2, password3, user1, user2, user3)
import Control.Exception (SomeException, throw, try)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT (..), NoLoggingT (..), runNoLoggingT, runStdoutLoggingT)
import qualified Data.Text as T
import Data.Time (UTCTime (..), addDays)
import Database.Esqueleto.Experimental (Key, OrderBy, PersistField (..), SqlExpr, Value (..), asc, count, delete, desc, from, fromSqlKey, get, getBy, groupBy, innerJoin, insert, insertMany, insertMany_, insert_, just, leftJoin, like, limit, offset, on, orderBy, replace, select, table, unionAll_, val, where_, withRecursive, (%), (&&.), (++.), (:&) (..), (<.), (==.), (>=.), (?.), (^.), (||.))
import Database.Persist.Postgresql (ConnectionString, Entity (..), rawExecute, toSqlKey, withPostgresqlConn)
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlConn)
import Handlers.Base (Content, Label, Login, Name, NewLabel, NewsOut, NumberImage, PasswordUser, Success (..), Title, URI_Image)
import Scheme (Category (..), ColumnType (..), EntityField (..), FilterItem (..), Find (..), Image (..), ImageBank (..), News (..), Password (..), SortOrder (..), Unique (..), User (..), migrateAll)

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
    insertMany_ [image1, image2, image3]
    insertMany_ [cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9]
    insertMany_ [password1, password2, password3]
    insertMany_ [user1, user2, user3]
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

validCopyRight :: ConnectionString -> Login -> Title -> IO (Either SomeException Bool)
validCopyRight connString login title = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadFail m, MonadIO m) => SqlPersistT m Bool
    fetchAction = do
      logins <- fetchUserFromNews
      case logins of
        [Value loginNews] -> pure (login == loginNews)
        _ -> pure False -- noValid
      where
        fetchUserFromNews :: (MonadIO m) => SqlPersistT m [Value Login]
        fetchUserFromNews = select $ do
          (news :& user) <-
            from $
              table @News
                `innerJoin` table @User
                  `on` (\(n :& u) -> n ^. NewsUserId ==. (u ^. UserId))
          where_ (news ^. NewsTitle ==. val title)
          pure (user ^. UserLogin)

validPassword :: ConnectionString -> Login -> PasswordUser -> IO (Either SomeException Bool)
validPassword connString login password = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadFail m, MonadIO m) => SqlPersistT m Bool
    fetchAction = do
      qpass <- fetchSaltAndPassword
      case qpass of
        [Value qpass'] -> pure $ Base.Crypto.validPassword password qpass'
        _ -> pure False -- noValid
    fetchSaltAndPassword :: (MonadFail m, MonadIO m) => SqlPersistT m [Value T.Text]
    fetchSaltAndPassword = select $ do
      (user :& pass) <-
        from $
          table @User
            `innerJoin` table @Password
              `on` (\(u :& p) -> u ^. UserPasswordId ==. p ^. PasswordId)
      where_ (user ^. UserLogin ==. val login)
      pure (pass ^. PasswordQuasiPassword)

pullAllNews :: ConnectionString -> LimitData -> Offset -> Limit -> ColumnType -> SortOrder -> Maybe Find -> [FilterItem] -> IO (Either SomeException [NewsOut])
-- getAllNews connString l = runDataBaseWithLog connString fetchAction
pullAllNews connString configLimit userOffset userLimit columnType sortOrder mbFind filters = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadFail m, MonadIO m) => SqlPersistT m [NewsOut]
    fetchAction = do
      titles <-
        (fmap . fmap)
          unValue
          ( select $ do
              (news :& author :& categoryName :& imageBank) <-
                from $
                  table @News
                    `innerJoin` table @User
                      `on` (\(n :& a) -> n ^. NewsUserId ==. a ^. UserId)
                    `innerJoin` table @Category
                      `on` (\(n :& _ :& c) -> n ^. NewsCategoryId ==. c ^. CategoryId)
                    `leftJoin` table @ImageBank
                      `on` (\(n :& _ :& _ :& ib) -> just (n ^. NewsId) ==. ib ?. ImageBankNewsId)
              groupBy (news ^. NewsTitle, news ^. NewsCreated, author ^. UserName, categoryName ^. CategoryLabel, imageBank ?. ImageBankNewsId)
              -- search substring
              maybe
                (where_ (val True))
                ( \(Find text) ->
                    let subtext = (%) ++. val text ++. (%)
                     in where_
                          ( (news ^. NewsContent `like` subtext)
                              ||. (author ^. UserName `like` subtext)
                              ||. (categoryName ^. CategoryLabel `like` subtext)
                          )
                )
                mbFind
              -- filters
              mapM_ (filterAction news author categoryName) filters
              -- sortBy column and order
              orderBy $ case columnType of
                DataNews -> [order sortOrder (news ^. NewsCreated)]
                AuthorNews -> [order sortOrder (author ^. UserName)]
                CategoryName -> [order sortOrder (categoryName ^. CategoryLabel)]
                QuantityImages -> [order sortOrder (count (imageBank ?. ImageBankNewsId) :: SqlExpr (Value Int))]
              -- offset and limit news
              offset (fromIntegral userOffset)
              limit (fromIntegral $ min configLimit userLimit)
              -- return title
              pure (news ^. NewsTitle)
          )
      mapM (fetchFullNews configLimit userLimit) titles

    filterAction n a c filter' = case filter' of
      -- FilterDataAt day -> where_ (( utctDay <$> (n ^. NewsCreated) ) ==. val (day))
      FilterDataAt day ->
        where_
          ( (n ^. NewsCreated >=. val (UTCTime day 0))
              &&. (n ^. NewsCreated <. val (UTCTime (addDays 1 day) 0))
          )
      FilterDataUntil day -> where_ (n ^. NewsCreated <. val (UTCTime day 0))
      FilterDataSince day -> where_ (n ^. NewsCreated >=. val (UTCTime day 0))
      FilterAuthorName name -> where_ (a ^. UserName ==. val name)
      FilterCategoryLabel label -> where_ (c ^. CategoryLabel ==. val label)
      FilterTitleFind findText -> where_ (n ^. NewsTitle `like` (%) ++. val findText ++. (%))
      FilterContentFind findText -> where_ (n ^. NewsContent `like` (%) ++. val findText ++. (%))
      FilterPublishOrAuthor login ->
        where_
          ( (n ^. NewsIsPublish ==. val True) -- publish all, not publish only author
              ||. (just (a ^. UserLogin) ==. val login)
          )

    order :: (PersistField a) => SortOrder -> (SqlExpr (Value a) -> SqlExpr OrderBy)
    order a = case a of
      Ascending -> asc
      Descending -> desc

fetchFullNews :: (MonadFail m, MonadIO m) => LimitData -> Limit -> Title -> SqlPersistT m NewsOut
fetchFullNews configLimit userLimit title = do
  (label : _) <- (fmap . fmap) entityVal fetchLabel
  lables <- fetchLables (categoryLabel label)
  (user : _) <- (fmap . fmap) entityVal fetchUser
  images <- fetchActionImage
  (Just partNews) <- (fmap . fmap) entityVal (getBy $ UniqueNews title)
  let a =
        ( title,
          newsCreated partNews,
          userName user,
          workerCategory lables,
          newsContent partNews,
          workerImage images,
          newsIsPublish partNews
        )
  pure a
  where
    fetchLabel :: (MonadIO m) => SqlPersistT m [Entity Category]
    fetchLabel = select $ do
      (news :& category) <-
        from $
          table @News
            `innerJoin` table @Category
              `on` (\(n :& c) -> n ^. NewsCategoryId ==. (c ^. CategoryId))
      where_ (news ^. NewsTitle ==. val title)
      pure category

    fetchUser :: (MonadIO m) => SqlPersistT m [Entity User]
    fetchUser = select $ do
      (news :& user) <-
        from $
          table @News
            `innerJoin` table @User
              `on` (\(n :& c) -> n ^. NewsUserId ==. (c ^. UserId))
      where_ (news ^. NewsTitle ==. val title)
      pure user

    fetchLables :: (MonadIO m) => Label -> SqlPersistT m [Entity Category]
    fetchLables label =
      select $ do
        cte <-
          withRecursive
            ( do
                child <- from $ table @Category
                where_ (child ^. CategoryLabel ==. val label)
                pure child
            )
            unionAll_
            ( \self -> do
                child <- from self
                parent <- from $ table @Category
                where_ (just (parent ^. CategoryId) ==. child ^. CategoryParent)
                pure parent
            )
        limit (fromIntegral $ min configLimit userLimit)
        from cte

    fetchActionImage :: (MonadIO m) => SqlPersistT m [Entity Image]
    fetchActionImage = select $ do
      (news :& _imagebank :& image) <-
        from $
          table @News
            `innerJoin` table @ImageBank
              `on` (\(n :& i) -> n ^. NewsId ==. (i ^. ImageBankNewsId))
            `innerJoin` table @Image
              `on` (\(_ :& i :& im) -> (i ^. ImageBankImageId) ==. (im ^. ImageId))
      where_ (news ^. NewsTitle ==. val title)
      limit (fromIntegral $ min configLimit userLimit)
      pure image

    workerImage :: [Entity Image] -> [URI_Image]
    workerImage = map (\(Entity key _value) -> "/images?id=" <> T.pack (show $ fromSqlKey key))

    workerCategory :: [Entity Category] -> [Label]
    workerCategory = map (\(Entity _key value) -> categoryLabel value)

findNewsByTitle :: ConnectionString -> Title -> IO (Either SomeException (Maybe News))
findNewsByTitle connString title = try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe News)
    fetchAction = (fmap . fmap) entityVal (getBy $ UniqueNews title)

editNews :: ConnectionString -> Title -> UTCTime -> Maybe Title -> Maybe Login -> Maybe Label -> Maybe Content -> [Image] -> Maybe Bool -> IO (Either SomeException Success)
editNews pginfo title time newTitle newLogin newLabel newContent newImages newPublish =
  try @SomeException
    ( runDataBaseWithOutLog pginfo $ do
        oldNews <- getBy $ UniqueNews title
        case oldNews of
          Just oldNews' -> do
            let (News oldTitle _oldTime oldKeyUser oldKeyCategory oldContent oldPublish) = entityVal oldNews'
            let keyNews = entityKey oldNews'
            newKeyUser <- case newLogin of
              Just login -> (fmap . fmap) entityKey (getBy $ UniqueUserLogin login)
              _ -> pure Nothing
            newKeyCategory <- case newLabel of
              Just label -> (fmap . fmap) entityKey (getBy $ UniqueCategoryLabel label)
              _ -> pure Nothing
            let newNews =
                  News
                    (replaceField oldTitle newTitle)
                    time
                    (replaceField oldKeyUser newKeyUser)
                    (replaceField oldKeyCategory newKeyCategory)
                    (replaceField oldContent newContent)
                    (replaceField oldPublish newPublish)
            replace keyNews newNews
            deleteImagesFromBankByNews keyNews
            keysImages <- insertMany newImages
            insertMany_ $ map (ImageBank keyNews) keysImages
            pure Change
          Nothing -> throw $ userError "function editNews fail (can't find news)"
    )
  where
    replaceField :: a -> Maybe a -> a
    replaceField _ (Just a) = a
    replaceField a _ = a

deleteImagesFromBankByNews :: (MonadIO m) => Key News -> SqlPersistT m ()
deleteImagesFromBankByNews key =
  delete $ do
    imageBank <- from $ table @ImageBank
    where_ (imageBank ^. ImageBankNewsId ==. val key)

putNews :: ConnectionString -> Title -> UTCTime -> Login -> Label -> Content -> [Image] -> Bool -> IO (Either SomeException Success)
putNews pginfo title time login label content images ispublish =
  try @SomeException
    ( runDataBaseWithOutLog pginfo $ do
        keyUser <- (fmap . fmap) entityKey (getBy $ UniqueUserLogin login)
        keyCategory <- (fmap . fmap) entityKey (getBy $ UniqueCategoryLabel label)
        case (keyUser, keyCategory) of
          (Just keyUsr, Just keyCat) -> do
            keyNews <- insert $ News title time keyUsr keyCat content ispublish
            keysImages <- insertMany images
            insertMany_ $ map (ImageBank keyNews) keysImages
            pure Put
          _ -> throw $ userError "function putNews fail"
    )

------------------------------------------------------------------------------------------------------------

pullImage :: ConnectionString -> NumberImage -> IO (Either SomeException (Maybe Image))
pullImage connString uid = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe Image)
    fetchAction = get (toSqlKey uid)

--
putUser :: ConnectionString -> Name -> Login -> PasswordUser -> UTCTime -> Bool -> Bool -> IO (Either SomeException Success)
putUser pginfo name login pwd time admin publish = do
  try @SomeException
    ( runDataBaseWithOutLog pginfo $ do
        -- runDataBaseWithLog pginfo $ do
        pId <- insert $ Password pwd
        _ <- insert $ User name login pId time admin publish
        pure Put
    )

findUserByLogin :: ConnectionString -> Login -> IO (Either SomeException (Maybe User))
-- findUserByLogin connString login = runDataBaseWithLog connString fetchAction
findUserByLogin connString login = try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe User)
    fetchAction = (fmap . fmap) entityVal (getBy $ UniqueUserLogin login)

type LimitData = Int

type Offset = Int

type Limit = Int

pullAllUsers :: ConnectionString -> LimitData -> Offset -> Limit -> IO (Either SomeException [User])
pullAllUsers connString configLimit userOffset userLimit = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m [User]
    fetchAction =
      (fmap . fmap)
        entityVal
        ( select $ do
            users <- from $ table @User
            offset (fromIntegral userOffset)
            limit (fromIntegral $ min configLimit userLimit)
            pure users
        )

------------------------------------------------------------------------------------------------------------

putCategory :: ConnectionString -> Label -> Maybe Label -> IO (Either SomeException Success)
putCategory pginfo label parent =
  try @SomeException
    ( runDataBaseWithOutLog pginfo $ do
        -- runDataBaseWithLog pginfo $ do
        case parent of
          Nothing -> do
            insert_ $ Category label Nothing
            pure Put
          Just labelParent -> do
            parentId <- (fmap . fmap) entityKey <$> getBy $ UniqueCategoryLabel labelParent
            insert_ $ Category label parentId
            pure Put
    )

--
editCategory :: ConnectionString -> Label -> NewLabel -> Maybe Label -> IO (Either SomeException Success)
editCategory pginfo label newLabel parent = do
  try @SomeException
    ( runDataBaseWithOutLog pginfo $ do
        -- runDataBaseWithLog pginfo $ do
        labelId <- (fmap . fmap) entityKey <$> getBy $ UniqueCategoryLabel label
        case (labelId, parent) of
          (Just iD, Nothing) -> replace iD $ Category newLabel Nothing
          (Just iD, Just labelParent) -> do
            parentId <- (fmap . fmap) entityKey <$> getBy $ UniqueCategoryLabel labelParent
            replace iD $ Category newLabel parentId
          _ -> throw $ userError "function editCategory fail (can't find category)" -- label don't exist
        pure Change
    )

findCategoryByLabel :: ConnectionString -> Label -> IO (Either SomeException (Maybe Category))
findCategoryByLabel connString label = try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    -- findCategoryByLabel connString label = runDataBaseWithLog connString fetchAction

    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe Category)
    fetchAction = (fmap . fmap) entityVal (getBy $ UniqueCategoryLabel label)

pullAllCategories :: ConnectionString -> LimitData -> Offset -> Limit -> IO (Either SomeException [Category])
-- pullAllCategories connString l = runDataBaseWithLog connString fetchAction
pullAllCategories connString configLimit userOffset userLimit = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m [Category]
    fetchAction =
      (fmap . fmap)
        entityVal
        ( select $ do
            categories <- from $ table @Category
            offset (fromIntegral userOffset)
            limit (fromIntegral $ min configLimit userLimit)
            pure categories
        )

------------------------------------------------------------------------------------------------------------
