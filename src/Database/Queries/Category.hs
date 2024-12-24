{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Category (pullAllCategories, findCategoryByLabel, putCategory, editCategory, findCategoryById, editCategoryId) where

import Control.Exception (SomeException, throw, try)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO)
import Database.Esqueleto.Experimental (from, get, getBy, insert_, limit, offset, replace, select, table)
import Database.Persist.Postgresql (ConnectionString, Entity (..))
import Database.Persist.Sql (SqlPersistT, toSqlKey)
import Database.Verb (runDataBaseWithOutLog)
import Handlers.Database.Base (Limit (..), Offset (..), Success (..))
import Handlers.Web.Base (CategoryInternal (..), CategoryInternalId (..))
import Schema (Category (..), Unique (..))
import Types (Label (..), NumberCategory (..))

type LimitData = Int

editCategory :: ConnectionString -> Label -> CategoryInternal -> IO (Either SomeException Success)
editCategory pginfo label (CategoryInternal newLabel parent) = do
  try @SomeException
    ( runDataBaseWithOutLog pginfo $ do
        labelId <- (fmap . fmap) entityKey <$> getBy . UniqueCategoryLabel . getLabel $ label
        case (labelId, parent) of
          (Just iD, Nothing) -> replace iD $ Category {categoryLabel = getLabel newLabel, categoryParent = Nothing}
          (Just iD, Just (MkLabel labelParent)) -> do
            parentId <- (fmap . fmap) entityKey <$> getBy . UniqueCategoryLabel $ labelParent
            replace iD $ Category {categoryLabel = getLabel newLabel, categoryParent = parentId}
          _ -> throw $ userError "function editCategory fail (can't find category)"
        pure Change
    )

findCategoryByLabel :: ConnectionString -> Label -> IO (Either SomeException (Maybe Category))
findCategoryByLabel connString label = try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe Category)
    fetchAction = (fmap . fmap) entityVal (getBy . UniqueCategoryLabel . getLabel $ label)

pullAllCategories :: ConnectionString -> LimitData -> Offset -> Limit -> IO (Either SomeException [Category])
pullAllCategories connString configLimit userOffset userLimit = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m [Category]
    fetchAction =
      (fmap . fmap)
        entityVal
        ( select $ do
            categories <- from $ table @Category
            offset (fromIntegral . getOffset $ userOffset)
            limit (fromIntegral . min configLimit . getLimit $ userLimit)
            pure categories
        )

putCategory :: ConnectionString -> CategoryInternal -> IO (Either SomeException Success)
putCategory pginfo (CategoryInternal {..}) =
  try @SomeException
    ( runDataBaseWithOutLog pginfo $ do
        case parentCategory of
          Nothing -> do
            insert_ $
              Category
                { categoryLabel = getLabel labelCategory,
                  categoryParent = Nothing
                }
            pure Put
          Just (MkLabel labelParent) -> do
            parentId <- (fmap . fmap) entityKey <$> getBy $ UniqueCategoryLabel labelParent
            insert_ $
              Category
                { categoryLabel = getLabel labelCategory,
                  categoryParent = parentId
                }
            pure Put
    )

findCategoryById :: ConnectionString -> NumberCategory -> IO (Either SomeException (Maybe Category))
findCategoryById connString (MkNumberCategory uid) = try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe Category)
    fetchAction = get (toSqlKey uid)

editCategoryId :: ConnectionString -> CategoryInternalId -> IO (Either SomeException Success)
editCategoryId pginfo (CategoryInternalId {..}) = do
  try @SomeException
    ( runDataBaseWithOutLog pginfo $ do
        let uid = toSqlKey $ getNumberCategory idCategory
        case (labelCategory, parentCategory) of
          (Just label, Nothing) -> do
            parentId <- (fmap . fmap) categoryParent (get uid)
            replace uid $ Category {categoryLabel = getLabel label, categoryParent = join parentId}
          (Just label, Just (MkLabel labelParent)) -> do
            parentId <- (fmap . fmap) entityKey <$> getBy . UniqueCategoryLabel $ labelParent
            replace uid $ Category {categoryLabel = getLabel label, categoryParent = parentId}
          (Nothing, Just (MkLabel labelParent)) -> do
            parentId <- (fmap . fmap) entityKey <$> getBy . UniqueCategoryLabel $ labelParent
            (Just label) <- (fmap . fmap) categoryLabel (get uid)
            replace uid $ Category {categoryLabel = label, categoryParent = parentId}
          _ -> throw $ userError "function editCategory fail (can't find category)"
        pure Change
    )
