module Handlers.Database.News.Get (getAllNews) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Either (isLeft)
import qualified Data.Text as T
import Handlers.Database.Base (Limit (..), Offset (..))
import Handlers.Database.News (Handle (..))
import Handlers.Logger (Log (..), logMessage)
import Handlers.Web.Base (NewsOut (..))
import Schema (ColumnType (..), FilterItem (..), Find (..), SortOrder (..))

getAllNews :: (Monad m) => Handle m -> Offset -> Limit -> ColumnType -> SortOrder -> Maybe Find -> [FilterItem] -> m (Either T.Text [NewsOut])
getAllNews h offset limit sortColumn sortOrder find filters = do
  let logHandle = logger h
  news <- pullAllNews h offset limit sortColumn sortOrder find filters
  when (isLeft news) (logMessage logHandle Handlers.Logger.Error "function pullAllNews fail")
  pure $ either (Left . T.pack . displayException) Right news
