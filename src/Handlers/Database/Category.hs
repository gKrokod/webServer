module Handlers.Database.Category (Handle (..)) where

import Handlers.Database.Base (Offset (..), Limit(..), Success(..))
import Control.Exception (SomeException)
import qualified Handlers.Logger
import Handlers.Web.Category.Types (CategoryInternal (..))
import Schema (Category(..))
import Types (Label)

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    userOffset :: Int,
    userLimit :: Int,
    pullAllCategories :: Offset -> Limit -> m (Either SomeException [Category]),
    findCategoryByLabel :: Label -> m (Either SomeException (Maybe Category)),
    putCategory :: CategoryInternal -> m (Either SomeException Success),
    editCategory :: Label -> CategoryInternal -> m (Either SomeException Success)
  }
