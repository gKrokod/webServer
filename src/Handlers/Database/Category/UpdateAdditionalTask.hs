module Handlers.Database.Category.UpdateAdditionalTask (updateCategoryBaseAdditionalTask) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Either (isLeft)
import qualified Data.Text as T
import Handlers.Database.Base (Handle (..), Success (..))
import Handlers.Logger (Log (..), logMessage)
import Handlers.Web.Base (CategoryInternalId (..))

updateCategoryBaseAdditionalTask :: (Monad m) => Handle m -> CategoryInternalId -> m (Either T.Text Success)
updateCategoryBaseAdditionalTask h newCat@(CategoryInternalId uid _ _) = do
  let logHandle = logger h
  exist <- findCategoryById h uid
  when (isLeft exist) (logMessage logHandle Handlers.Logger.Error "function findCategoryById fail")
  tryEdit <- editCategoryId h newCat
  when (isLeft tryEdit) (logMessage logHandle Handlers.Logger.Error "Can't edit Category")
  pure $ either (Left . T.pack . displayException) Right tryEdit
