{-# LANGUAGE RecordWildCards #-}

module Handlers.Database.Category.Create (createCategoryBase) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Either (isLeft)
import qualified Data.Text as T
import Handlers.Database.Base (Handle (..), Success (..))
import Handlers.Logger (Log (..), logMessage)
import Handlers.Web.Base (CategoryInternal (..))
import Types (Label (..))

createCategoryBase :: (Monad m) => Handle m -> CategoryInternal -> m (Either T.Text Success)
createCategoryBase h cat@(CategoryInternal {..}) = do
  let logHandle = logger h
  exist <- findCategoryByLabel h labelCategory
  case (exist, parentCategory) of
    (Left e, _) -> do
      logMessage logHandle Error "function findCategoryByLabel fail"
      pure . Left . T.pack . displayException $ e
    (Right (Just _), _) -> do
      logMessage logHandle Warning ("Category arleady taken: " <> getLabel labelCategory)
      pure $ Left "Category arleady taken"
    (Right Nothing, Nothing) -> do
      tryPut <- putCategory h cat
      when (isLeft tryPut) (logMessage logHandle Handlers.Logger.Error "function putCategory fali")
      pure $ either (Left . T.pack . displayException) Right tryPut
    (Right Nothing, Just labelParent) -> do
      existLabel <- findCategoryByLabel h labelParent
      case existLabel of
        Left e -> do
          logMessage logHandle Error "function findCategoryByLabel fail"
          pure . Left . T.pack . displayException $ e
        Right Nothing -> do
          logMessage logHandle Warning ("Abort. Parent dont' exist: " <> getLabel labelParent)
          pure $ Left "Parent dont' exist"
        _ -> do
          tryPut <- putCategory h cat
          when (isLeft tryPut) (logMessage logHandle Handlers.Logger.Error "function putCategory fail")
          pure $ either (Left . T.pack . displayException) Right tryPut
