module Handlers.Web.User.UserApi (endPointUsers) where

import qualified Handlers.Database.User
import qualified Handlers.Web.User
import Handlers.Logger (logMessage, Log(Warning))
import Handlers.Web.Base (Client (..), Handle (..))
import Handlers.Web.User.Create (createUser)
import Handlers.Web.User.Get (existingUsers)
import Network.Wai (Request, Response, queryString, rawPathInfo)
import Web.Query (queryToPaginate)
import Data.Time (UTCTime)
import qualified Handlers.Logger
import Handlers.Web.Category.Types (CategoryInternal (..))
import Handlers.Web.User.Types (UserInternal (..))
import Handlers.Web.News.Types (NewsEditInternal (..), NewsInternal (..), NewsOut (..))
import Schema (Category (..), ColumnType (..), FilterItem (..), Find (..), Image (..), News (..), SortOrder (..), User (..))
import Types (Label (..), Login (..), NumberImage (..), PasswordUser (..), Title (..))

endPointUsers :: (Monad m) => Handle m -> Request -> m Response
endPointUsers h req = do
  let logHandle = Handlers.Web.Base.logger h
      userHandle = Handlers.Web.Base.user h
      baseUserHandle = Handlers.Web.User.base userHandle
      userRole = Handlers.Web.Base.client h
  case rawPathInfo req of
    "/users/create" -> do
      case userRole of
        Client {clientAdminToken = (Just adminRole)} -> createUser adminRole userHandle req
        _ -> do
          logMessage logHandle Warning "Access denied"
          pure $ Handlers.Web.User.response404 userHandle
    "/users" -> do
      let queryLimit = queryString req
          (userOffset, userLimit) = queryToPaginate queryLimit
      let newBaseUserHandle = baseUserHandle {Handlers.Database.User.userOffset = userOffset, Handlers.Database.User.userLimit = userLimit}
      existingUsers (userHandle {Handlers.Web.User.base = newBaseUserHandle} ) req
    _ -> do
      logMessage logHandle Warning "End point Users not found"
      pure $ Handlers.Web.User.response404 userHandle
