module Handlers.Database.News (Handle (..)) where

import Control.Exception (SomeException)
import Database.Persist.Sql (PersistentSqlException)
import Data.Time (UTCTime)
import Handlers.Database.Base (Limit (..), Offset (..), Success (..))
import qualified Handlers.Logger
import Handlers.Web.News.Types (NewsEditInternal (..), NewsInternal (..), NewsOut (..))
import Schema (Category (..), ColumnType (..), FilterItem (..), Find (..), News (..), SortOrder (..), User (..))
import Types (Label (..), Login (..), Title (..))

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    findNewsByTitle :: Title -> m (Either SomeException (Maybe News)),
    getTime :: m UTCTime,
    putNews :: NewsInternal -> UTCTime -> m (Either SomeException Success),
    editNews :: Title -> UTCTime -> NewsEditInternal -> m (Either SomeException Success),
    findUserByLogin :: Login -> m (Either SomeException (Maybe User)),
    findCategoryByLabel :: Label -> m (Either PersistentSqlException (Maybe Category)),
    pullAllNews :: Offset -> Limit -> ColumnType -> SortOrder -> Maybe Find -> [FilterItem] -> m (Either SomeException [NewsOut])
  }
