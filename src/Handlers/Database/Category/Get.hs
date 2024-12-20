module Handlers.Database.Category.Get (getAllCategories) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Either (isLeft)
import qualified Data.Text as T
import Handlers.Database.Base (Handle (..), Limit (..), Offset (..))
import Handlers.Logger (Log (..), logMessage)
import Schema (Category (..))

getAllCategories :: (Monad m) => Handle m -> m (Either T.Text [Category])
getAllCategories h = do
  let logHandle = logger h
  categories <- pullAllCategories h (MkOffset . userOffset $ h) (MkLimit . userLimit $ h)
  when (isLeft categories) (logMessage logHandle Handlers.Logger.Error "function pullAllCategories fail")
  pure $ either (Left . T.pack . displayException) Right categories
