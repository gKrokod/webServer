module Handlers.Web.User.UserApi (endPointUsers) where

import qualified Handlers.Database.Base
import qualified Handlers.Logger
import Handlers.Web.Base (Client (..), Handle (..))
import Handlers.Web.Base (HandleUser(..))
import Handlers.Web.User.Create (createUser)
import Handlers.Web.User.Get (existingUsers)
import Network.Wai (Request, Response, queryString, rawPathInfo)
import Web.Query (queryToPaginate)
import qualified Database.Api as DA
import qualified Web.Utils as WU

endPointUsers :: (Monad m) => Handle m -> Request -> m Response
endPointUsers h req = do
  let logHandle = logger h
  case rawPathInfo req of
    "/users/create" -> do
      case client h of
        Client {clientAdminToken = (Just adminRole)} -> createUser adminRole h' req
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure $ response404 h
    "/users" -> do
      let queryLimit = queryString req
          (userOffset, userLimit) = queryToPaginate queryLimit

      -- let newBaseHandle = baseHandle {Handlers.Database.Base.userOffset = userOffset, Handlers.Database.Base.userLimit = userLimit}
      let newUserHandle = h' {userOffsetU = userOffset, userLimitU = userLimit}
      existingUsers newUserHandle req
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point Users not found"
      pure $ response404 h
  where h' = HandleUser {
        response400U = WU.response400,
        response500U = WU.response500,
        response200U = WU.response200,
        userOffsetU = Handlers.Database.Base.userOffset $ base h,
        userLimitU = Handlers.Database.Base.userLimit $ base h,
        -- getBodyU = WU.getBody,
        getBodyU = getBody h,
        getTime = Handlers.Database.Base.getTime $ base h,
        mkGoodResponseU = WU.mkGoodResponse,
        pullAllUsers = Handlers.Database.Base.pullAllUsers $ base h,
        findUserByLogin = Handlers.Database.Base.findUserByLogin $ base h,
        putUser = Handlers.Database.Base.putUser $ base h,
        makeHashPassword = DA.makeHashPassword,
        loggerUser = logger h
        }

-- data HandleUser m = HandleUser
--   { 
--     response400U :: Text -> Response,
--     response500U :: Response,
--     response200U :: Response,
--     mkGoodResponseU :: Builder -> Response,
--     pullAllUsers :: Offset -> Limit -> m (Either SomeException [User]),
--     findUserByLogin :: Login -> m (Either SomeException (Maybe User)),
--     makeHashPassword :: PasswordUser -> UTCTime -> HashPasswordUser,
--     putUser :: UserInternal -> UTCTime -> m (Either SomeException Success),
--     loggerUser :: Handlers.Logger.Handle m
--   }        

-- endPointImages :: (Monad m) => Handle m -> Request -> m Response
-- endPointImages h req = do
--   case rawPathInfo req of
--     "/images" -> existingImages h' req
--     _ -> do
--       logMessage (logger h)  Warning "End point not found"
--       pure $ response404 h
--   where h' = HandleImage { 
--                 response400B = WU.response400, 
--                 response500B = WU.response500,
--                 mkResponseForImage = WU.mkResponseForImage,
--                 pullImage = Handlers.Database.Base.pullImage $ base h,
--                 loggerImage = logger h}
