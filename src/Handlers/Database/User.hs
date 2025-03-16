module Handlers.Database.User (Handle (..)) where

import Control.Exception (SomeException)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Handlers.Database.Base (Limit (..), Offset (..), Success (..))
import qualified Handlers.Logger
import Handlers.Web.User.Types (UserInternal (..))
import Schema (User (..))
import Types (Login (..), PasswordUser (..))

type HashPasswordUser = T.Text

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    userOffset :: Int,
    userLimit :: Int,
    getTime :: m UTCTime,
    makeHashPassword :: PasswordUser -> UTCTime -> HashPasswordUser,
    pullAllUsers :: Offset -> Limit -> m (Either SomeException [User]),
    findUserByLogin :: Login -> m (Either SomeException (Maybe User)),
    putUser :: UserInternal -> UTCTime -> m (Either SomeException Success)
  }
