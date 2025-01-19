module Handlers.Database.Category.Update (updateCategoryBase) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Either (isLeft)
import qualified Data.Text as T
import Handlers.Database.Base ( Success (..))
import Handlers.Database.Category (Handle (..))
import Handlers.Logger (Log (..), logMessage)
import Handlers.Web.Base (CategoryInternal (..))
import Types (Label (..))

updateCategoryBase :: (Monad m) => Handle m -> Label -> CategoryInternal -> m (Either T.Text Success)
updateCategoryBase h label newCat@(CategoryInternal newlabel parent) = do
  let logHandle = logger h
  exist <- findCategoryByLabel h label
  when (isLeft exist) (logMessage logHandle Handlers.Logger.Error "function findCategoryByLabel fail")

  existNew <- findCategoryByLabel h newlabel
  when (isLeft existNew) (logMessage logHandle Handlers.Logger.Error "function findCategoryByLabel fail")
  let existNew' = if label == newlabel then Right Nothing else existNew
  case (sequence [exist, existNew'], parent) of
    (Left e, _) -> pure . Left . T.pack . displayException $ e
    (Right [Just _, Nothing], Nothing) -> do
      tryEdit <- editCategory h label newCat
      when (isLeft tryEdit) (logMessage logHandle Handlers.Logger.Error "Can't editCategory")
      pure $ either (Left . T.pack . displayException) Right tryEdit
    (Right [Just _, Nothing], Just labelParent) -> do
      existLabel <- findCategoryByLabel h labelParent
      case existLabel of
        Left e -> do
          logMessage logHandle Handlers.Logger.Error "function findCategoryByLabel fail"
          pure . Left . T.pack . displayException $ e
        Right Nothing -> do
          logMessage logHandle Warning ("Abort. Parent don't exist: " <> getLabel labelParent)
          pure $ Left "Parent dont' exist"
        Right (Just _) -> do
          tryEdit <- editCategory h label newCat
          when (isLeft tryEdit) (logMessage logHandle Handlers.Logger.Error "Can't editCategory")
          pure $ either (Left . T.pack . displayException) Right tryEdit
    _ -> do
      logMessage logHandle Warning ("Abort. Category don't exist or .... Category: " <> getLabel label)
      pure $ Left "Category don't's exist or ..."
