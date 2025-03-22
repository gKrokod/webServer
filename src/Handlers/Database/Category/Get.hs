module Handlers.Database.Category.Get (getAllCategories) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Either (isLeft)
import qualified Data.Text as T
import Handlers.Database.Base (Limit (..), Offset (..))
import Handlers.Database.Category (Handle (..))
import Handlers.Logger (Log (..), logMessage)
import Schema (Category (..))
--
getAllCategories :: (Monad m) => Handle m -> Offset -> Limit -> m (Either T.Text [Category])
getAllCategories h offset limit = do
  let logHandle = logger h
  categories <- pullAllCategories h offset limit 
  when (isLeft categories) (logMessage logHandle Handlers.Logger.Error "function pullAllCategories fail")
  pure $ either (Left . T.pack . displayException) Right categories
