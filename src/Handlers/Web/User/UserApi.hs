module Handlers.Web.User.UserApi (endPointUsers) where

import Handlers.Database.Auth (Client (..))
import Handlers.Logger (Log (Warning), logMessage)
import Handlers.Web.Base (Handle (..))
import Handlers.Web.User.Create (createUser)
import Handlers.Web.User.Get (existingUsers)
import Network.Wai (Request, Response, rawPathInfo)
import qualified Web.Utils as WU

endPointUsers :: (Monad m) => Handle m -> Request -> m Response
endPointUsers h req = do
  let logHandle = Handlers.Web.Base.logger h
      userHandle = Handlers.Web.Base.user h
      userRole = Handlers.Web.Base.client h
  case rawPathInfo req of
    "/users/create" -> do
      case userRole of
        Client {clientAdminToken = (Just adminRole)} -> createUser adminRole userHandle req
        _ -> do
          logMessage logHandle Warning "Access denied"
          pure WU.response404
    "/users" -> existingUsers userHandle req
    _ -> do
      logMessage logHandle Warning "End point Users not found"
      pure WU.response404
