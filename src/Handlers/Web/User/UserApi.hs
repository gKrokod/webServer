module Handlers.Web.User.UserApi (endPointUsers) where

import qualified Handlers.Database.Base
import qualified Handlers.Database.User
import qualified Handlers.Web.User
import qualified Handlers.Logger
import Handlers.Web.Base (Client (..), Handle (..))
import Handlers.Web.User.Create (createUser)
import Handlers.Web.User.Get (existingUsers)
import Network.Wai (Request, Response, queryString, rawPathInfo)
import Web.Query (queryToPaginate)
import qualified Database.Api as DA
import qualified Web.Utils as WU

endPointUsers :: (Monad m) => Handle m -> Request -> m Response
endPointUsers h req = do
  let logHandle = Handlers.Web.Base.logger h
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

      let newBaseUserHandle = baseHandle {Handlers.Database.User.userOffset = userOffset, Handlers.Database.User.userLimit = userLimit}
      existingUsers (h' {Handlers.Web.User.base = newBaseUserHandle} ) req
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point Users not found"
      pure $ response404 h
  where baseHandle = Handlers.Database.User.Handle {
          Handlers.Database.User.logger = Handlers.Web.Base.logger h,
          Handlers.Database.User.userOffset = Handlers.Database.Base.userOffset $ base h,
          Handlers.Database.User.userLimit = Handlers.Database.Base.userLimit $ base h,
          Handlers.Database.User.getTime = Handlers.Database.Base.getTime $ base h,
          Handlers.Database.User.makeHashPassword = DA.makeHashPassword,
          Handlers.Database.User.pullAllUsers = Handlers.Database.Base.pullAllUsers $ base h,
          Handlers.Database.User.findUserByLogin = Handlers.Database.Base.findUserByLogin $ base h,
          Handlers.Database.User.putUser = Handlers.Database.Base.putUser $ Handlers.Web.Base.base h
        }
        h' = Handlers.Web.User.Handle {
          Handlers.Web.User.logger = Handlers.Web.Base.logger h,
          Handlers.Web.User.base = baseHandle,
          Handlers.Web.User.response400 = WU.response400,
          Handlers.Web.User.response500 = WU.response500,
          Handlers.Web.User.response200 = WU.response200,
          Handlers.Web.User.getBody = Handlers.Web.Base.getBody h,
          Handlers.Web.User.mkGoodResponse = WU.mkGoodResponse
        }

-- data Handle m = Handle
--   { connectionString :: ConnectionString, 
--     logger :: Handlers.Logger.Handle m,
--     base :: Handlers.Database.User.Handle m,
--     getBody :: Request -> m B.ByteString,
--     response200 :: Response,
--     response400 :: Text -> Response,
--     response500 :: Response,
--     mkGoodResponse :: Builder -> Response
--   }
-- data Handle m = Handle
--   { connectionString :: ConnectionString, 
--     logger :: Handlers.Logger.Handle m,
--     base :: Handlers.Database.User.Handle m,
--     getBody :: Request -> m B.ByteString,
--     response200 :: Response,
--     response400 :: Text -> Response,
--     response500 :: Response,
--     mkGoodResponse :: Builder -> Response
--   }
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
