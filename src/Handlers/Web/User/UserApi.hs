module Handlers.Web.User.UserApi (endPointUsers) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Handlers.Database.Base
import qualified Handlers.Logger
import Handlers.Web.Base (Client (..), Handle (..))
import Handlers.Web.User.Create (createUser)
import Handlers.Web.User.Get (existingUsers)
import Network.Wai (Request, Response, queryString, rawPathInfo)
import Web.WebType (queryToPanigate)

endPointUsers :: (Monad m) => Handle m -> Request -> m Response
endPointUsers h req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "end Point Users"
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (E.decodeUtf8 $ rawPathInfo req)
  case rawPathInfo req of
    "/users/create" -> do
      case client h of
        Client {clientAdminToken = (Just adminRole)} -> createUser adminRole h req -- create User for only admin
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure (response404 h)
    "/users" -> do
      let queryLimit = queryString req
          (userOffset, userLimit) = queryToPanigate queryLimit
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Query String:"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryLimit)
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show (userOffset, userLimit))

      let newBaseHandle = baseHandle {Handlers.Database.Base.userOffset = userOffset, Handlers.Database.Base.userLimit = userLimit}
      existingUsers (h {base = newBaseHandle}) req
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point Users not found"
      pure (response404 h)
