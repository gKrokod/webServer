module Handlers.Web.User.UserApi (endPointUsers) where

import Handlers.Database.Auth (Client (..))
import qualified Handlers.Database.User
import Handlers.Logger (Log (Warning), logMessage)
import Handlers.Web.Base (Handle (..))
import qualified Handlers.Web.User
import Handlers.Web.User.Create (createUser)
import Handlers.Web.User.Get (existingUsers)
import Network.Wai (Request, Response, queryString, rawPathInfo)
import Web.Query (queryToPaginate)
import qualified Web.Utils as WU

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
          pure $ WU.response404
    "/users" -> do
      let queryLimit = queryString req
          (userOffset, userLimit) = queryToPaginate queryLimit
      let newBaseUserHandle = baseUserHandle {Handlers.Database.User.userOffset = userOffset, Handlers.Database.User.userLimit = userLimit}
      existingUsers (userHandle {Handlers.Web.User.base = newBaseUserHandle}) req
    _ -> do
      logMessage logHandle Warning "End point Users not found"
      pure $ WU.response404
