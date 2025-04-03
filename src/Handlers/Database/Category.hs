module Handlers.Database.Category (Handle (..)) where

import Handlers.Database.Base (Limit (..), Offset (..), Success (..))
import qualified Handlers.Logger
import Handlers.Web.Category.Types (CategoryInternal (..))
import Schema (Category (..))
import Types (Label)
import Database.Persist.Sql (PersistentSqlException)

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    pullAllCategories :: Offset -> Limit -> m (Either PersistentSqlException [Category]),
    findCategoryByLabel :: Label -> m (Either PersistentSqlException (Maybe Category)),
    putCategory :: CategoryInternal -> m (Either PersistentSqlException Success),
    editCategory :: Label -> CategoryInternal -> m (Either PersistentSqlException Success)
  }
