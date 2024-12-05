module Handlers.Web.User.UserApi (endPointUsers) where

import qualified Handlers.Database.Base
import qualified Handlers.Logger
import Handlers.Web.Base (Client (..), Handle (..))
import Handlers.Web.User.Create (createUser)
import Handlers.Web.User.Get (existingUsers)
import Network.Wai (Request, Response, queryString, rawPathInfo)
import Web.WebType (queryToPaginate)

endPointUsers :: (Monad m) => Handle m -> Request -> m Response
endPointUsers h req = do
  let logHandle = logger h
      baseHandle = base h
  case rawPathInfo req of
    "/users/create" -> do
      case client h of
        Client {clientAdminToken = (Just adminRole)} -> createUser adminRole h req
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure $ response404 h
    "/users" -> do
      let queryLimit = queryString req
          (userOffset, userLimit) = queryToPaginate queryLimit

      let newBaseHandle = baseHandle {Handlers.Database.Base.userOffset = userOffset, Handlers.Database.Base.userLimit = userLimit}
      existingUsers (h {base = newBaseHandle}) req
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point Users not found"
      pure $ response404 h
