{-# LANGUAGE RecordWildCards #-}

module Database.Queries.News (editNews, pullAllNews, putNews, findNewsByTitle) where

import Control.Exception (SomeException, throw, try)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Data.Time (UTCTime (..), addDays)
import Database.Esqueleto.Experimental (Key, OrderBy, PersistField (..), SqlExpr, Value (..), asc, count, delete, desc, from, fromSqlKey, getBy, groupBy, innerJoin, insert, insertMany, insertMany_, just, leftJoin, like, limit, offset, on, orderBy, replace, select, table, unionAll_, val, where_, withRecursive, (%), (&&.), (++.), (:&) (..), (<.), (==.), (>=.), (?.), (^.), (||.))
import Database.Persist.Postgresql (ConnectionString, Entity (..))
import Database.Persist.Sql (SqlPersistT)
import Database.Verb (runDataBaseWithOutLog)
import Handlers.Database.Base (Limit (..), Offset (..), Success (..))
import Handlers.Web.Base (NewsEditInternal (..), NewsInternal (..), NewsOut (..))
import Schema (Category (..), ColumnType (..), EntityField (..), FilterItem (..), Find (..), Image (..), ImageBank (..), News (..), SortOrder (..), Unique (..), User (..))
import Types (Content (..), Label (..), Login (..), Name (..), Title (..), URI_Image (..))

type LimitData = Int

pullAllNews :: ConnectionString -> LimitData -> Offset -> Limit -> ColumnType -> SortOrder -> Maybe Find -> [FilterItem] -> IO (Either SomeException [NewsOut])
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
                from
                  $ table @News
                    `innerJoin` table @User
                  `on` (\(n :& a) -> n ^. NewsUserId ==. a ^. UserId)
                    `innerJoin` table @Category
                  `on` (\(n :& _ :& c) -> n ^. NewsCategoryId ==. c ^. CategoryId)
                    `leftJoin` table @ImageBank
                  `on` (\(n :& _ :& _ :& ib) -> just (n ^. NewsId) ==. ib ?. ImageBankNewsId)
              groupBy (news ^. NewsTitle, news ^. NewsCreated, author ^. UserName, categoryName ^. CategoryLabel, imageBank ?. ImageBankNewsId)
              
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
              
              mapM_ (filterAction news author categoryName) filters

              orderBy $ case columnType of
                DataNews -> [order sortOrder (news ^. NewsCreated)]
                AuthorNews -> [order sortOrder (author ^. UserName)]
                CategoryName -> [order sortOrder (categoryName ^. CategoryLabel)]
                QuantityImages -> [order sortOrder (count (imageBank ?. ImageBankNewsId) :: SqlExpr (Value Int))]
              
              offset (fromIntegral . getOffset $ userOffset)
              limit (fromIntegral . min configLimit . getLimit $ userLimit)
              
              pure (news ^. NewsTitle)
          )
      mapM (fetchFullNews configLimit userLimit . MkTitle) titles

    filterAction n a c filter' = case filter' of
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
          ( (n ^. NewsIsPublish ==. val True) 
              ||. (just (a ^. UserLogin) ==. val login)
          )

    order :: (PersistField a) => SortOrder -> (SqlExpr (Value a) -> SqlExpr OrderBy)
    order a = case a of
      Ascending -> asc
      Descending -> desc

fetchFullNews :: (MonadFail m, MonadIO m) => LimitData -> Limit -> Title -> SqlPersistT m NewsOut
fetchFullNews configLimit userLimit title = do
  (label : _) <- (fmap . fmap) entityVal fetchLabel
  lables <- fetchLables (MkLabel $ categoryLabel label)
  (user : _) <- (fmap . fmap) entityVal fetchUser
  images <- fetchActionImage
  (Just partNews) <- (fmap . fmap) entityVal (getBy . UniqueNews . getTitle $ title)
  let a =
        MkNewsOut
          { nTitle = title,
            nTime = newsCreated partNews,
            nAuthor = MkName $ userName user,
            nCategories = workerCategory lables,
            nContent = MkContent $ newsContent partNews,
            nImages = workerImage images,
            nIsPublish = newsIsPublish partNews
          }
  pure a
  where
    fetchLabel :: (MonadIO m) => SqlPersistT m [Entity Category]
    fetchLabel = select $ do
      (news :& category) <-
        from
          $ table @News
            `innerJoin` table @Category
          `on` (\(n :& c) -> n ^. NewsCategoryId ==. (c ^. CategoryId))
      where_ (news ^. NewsTitle ==. (val . getTitle) title)
      pure category

    fetchUser :: (MonadIO m) => SqlPersistT m [Entity User]
    fetchUser = select $ do
      (news :& user) <-
        from
          $ table @News
            `innerJoin` table @User
          `on` (\(n :& c) -> n ^. NewsUserId ==. (c ^. UserId))
      where_ (news ^. NewsTitle ==. (val . getTitle) title)
      pure user

    fetchLables :: (MonadIO m) => Label -> SqlPersistT m [Entity Category]
    fetchLables label =
      select $ do
        cte <-
          withRecursive
            ( do
                child <- from $ table @Category
                where_ (child ^. CategoryLabel ==. (val . getLabel) label)
                pure child
            )
            unionAll_
            ( \self -> do
                child <- from self
                parent <- from $ table @Category
                where_ (just (parent ^. CategoryId) ==. child ^. CategoryParent)
                pure parent
            )
        limit (fromIntegral . min configLimit . getLimit $ userLimit)
        from cte

    fetchActionImage :: (MonadIO m) => SqlPersistT m [Entity Image]
    fetchActionImage = select $ do
      (news :& _imagebank :& image) <-
        from
          $ table @News
            `innerJoin` table @ImageBank
          `on` (\(n :& i) -> n ^. NewsId ==. (i ^. ImageBankNewsId))
            `innerJoin` table @Image
          `on` (\(_ :& i :& im) -> (i ^. ImageBankImageId) ==. (im ^. ImageId))
      where_ (news ^. NewsTitle ==. (val . getTitle) title)
      limit (fromIntegral . min configLimit . getLimit $ userLimit)
      pure image

    workerImage :: [Entity Image] -> [URI_Image]
    workerImage = map (\(Entity key _value) -> MkURI_Image $ "/images?id=" <> T.pack (show $ fromSqlKey key))

    workerCategory :: [Entity Category] -> [Label]
    workerCategory = map (\(Entity _key value) -> MkLabel $ categoryLabel value)

editNews :: ConnectionString -> Title -> UTCTime -> NewsEditInternal -> IO (Either SomeException Success)
editNews pginfo title time (NewsEditInternal newTitle newLogin newLabel newContent newImages newPublish) =
  try @SomeException
    ( runDataBaseWithOutLog pginfo $ do
        oldNews <- getBy . UniqueNews . getTitle $ title
        case oldNews of
          Just oldNews' -> do
            let (News oldTitle _oldTime oldKeyUser oldKeyCategory oldContent oldPublish) = entityVal oldNews'
            let keyNews = entityKey oldNews'
            newKeyUser <- case newLogin of
              Just (MkLogin login) -> (fmap . fmap) entityKey (getBy $ UniqueUserLogin login)
              _ -> pure Nothing
            newKeyCategory <- case newLabel of
              Just (MkLabel label) -> (fmap . fmap) entityKey (getBy $ UniqueCategoryLabel label)
              _ -> pure Nothing
            let newNews =
                  News
                    (replaceField oldTitle (fmap getTitle newTitle))
                    time
                    (replaceField oldKeyUser newKeyUser)
                    (replaceField oldKeyCategory newKeyCategory)
                    (replaceField oldContent (fmap getContent newContent))
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

putNews :: ConnectionString -> NewsInternal -> UTCTime -> IO (Either SomeException Success)
putNews pginfo (NewsInternal {..}) time =
  try @SomeException
    ( runDataBaseWithOutLog pginfo $ do
        keyUser <- (fmap . fmap) entityKey (getBy . UniqueUserLogin . getLogin $ authorNews)
        keyCategory <- (fmap . fmap) entityKey (getBy . UniqueCategoryLabel . getLabel $ labelNews)
        case (keyUser, keyCategory) of
          (Just keyUsr, Just keyCat) -> do
            keyNews <-
              insert $
                News
                  { newsTitle = getTitle titleNews,
                    newsCreated = time,
                    newsUserId = keyUsr,
                    newsCategoryId = keyCat,
                    newsContent = getContent contentNews,
                    newsIsPublish = isPublishNews
                  }
            keysImages <- insertMany imagesNews
            insertMany_ $ map (ImageBank keyNews) keysImages
            pure Put
          _ -> throw $ userError "function putNews fail"
    )

deleteImagesFromBankByNews :: (MonadIO m) => Key News -> SqlPersistT m ()
deleteImagesFromBankByNews key =
  delete $ do
    imageBank <- from $ table @ImageBank
    where_ (imageBank ^. ImageBankNewsId ==. val key)

findNewsByTitle :: ConnectionString -> Title -> IO (Either SomeException (Maybe News))
findNewsByTitle connString title = try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe News)
    fetchAction = (fmap . fmap) entityVal (getBy . UniqueNews . getTitle $ title)
