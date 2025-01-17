module Handlers.Database.Base (Handle (..), Success (..), Offset (..), Limit (..)) where

import Control.Exception (SomeException)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Handlers.Logger
import Handlers.Web.Category.Types (CategoryInternal (..))
import Handlers.Web.News.Types (NewsEditInternal (..), NewsInternal (..), NewsOut (..))
import Schema (Category (..), ColumnType (..), FilterItem (..), Find (..), Image (..), News (..), SortOrder (..), User (..))
import Types (Label (..), Login (..), NumberImage (..), PasswordUser (..), Title (..))

data Success = Put | Change | Get deriving (Show, Eq)

newtype Offset = MkOffset {getOffset :: Int}

newtype Limit = MkLimit {getLimit :: Int}

type HashPasswordUser = T.Text

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    userOffset :: Int,
    userLimit :: Int,
    sortColumnNews :: ColumnType,
    sortOrderNews :: SortOrder,
    findSubString :: Maybe Find,
    filtersNews :: [FilterItem],
    getTime :: m UTCTime,
    makeHashPassword :: PasswordUser -> UTCTime -> HashPasswordUser,
    validPassword :: Login -> PasswordUser -> m (Either SomeException Bool),
    validCopyRight :: Login -> Title -> m (Either SomeException Bool),
    -- pullAllUsers :: Offset -> Limit -> m (Either SomeException [User]),
    pullAllNews :: Offset -> Limit -> ColumnType -> SortOrder -> Maybe Find -> [FilterItem] -> m (Either SomeException [NewsOut]),
    pullAllCategories :: Offset -> Limit -> m (Either SomeException [Category]),
    pullImage :: NumberImage -> m (Either SomeException (Maybe Image)),
    findUserByLogin :: Login -> m (Either SomeException (Maybe User)),
    findCategoryByLabel :: Label -> m (Either SomeException (Maybe Category)),
    findNewsByTitle :: Title -> m (Either SomeException (Maybe News)),
    -- putUser :: UserInternal -> UTCTime -> m (Either SomeException Success),
    putCategory :: CategoryInternal -> m (Either SomeException Success),
    putNews :: NewsInternal -> UTCTime -> m (Either SomeException Success),
    editNews :: Title -> UTCTime -> NewsEditInternal -> m (Either SomeException Success),
    editCategory :: Label -> CategoryInternal -> m (Either SomeException Success)
  }
