module Handlers.Database.Category (Handle (..)) where

import Control.Exception (SomeException)
import Handlers.Database.Base (Limit (..), Offset (..), Success (..))
import qualified Handlers.Logger
import Handlers.Web.Category.Types (CategoryInternal (..))
import Schema (Category (..))
import Types (Label)

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    pullAllCategories :: Offset -> Limit -> m (Either SomeException [Category]),
    findCategoryByLabel :: Label -> m (Either SomeException (Maybe Category)),
    putCategory :: CategoryInternal -> m (Either SomeException Success),
    editCategory :: Label -> CategoryInternal -> m (Either SomeException Success)
  }
