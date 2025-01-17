-- {-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Handlers.Web.Base (Handle (..), Client (..), UserInternal (..), CategoryInternal (..), ClientRole (..), NewsInternal (..), NewsOut (..), NewsEditInternal (..), HandleImage(..)) where

import Data.Binary.Builder (Builder)
import qualified Data.ByteString as B
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Handlers.Database.Base
import qualified Handlers.Logger
import Handlers.Web.Category.Types (CategoryInternal (..))
import Handlers.Web.News.Types (NewsEditInternal (..), NewsInternal (..), NewsOut (..))
import Handlers.Web.User.Types (UserInternal (..))
import Network.Wai (Request, Response)
import Schema (Image)
import Types (Login (..))
import Types (NumberImage (..))
import Control.Exception (SomeException)
import Database.Persist.Postgresql (ConnectionString)
import qualified Handlers.Web.User (Handle (..))


data ClientRole = AdminRole | PublisherRole
  deriving (Eq, Show)

data Client = Client
  { clientAdminToken :: Maybe (Proxy 'AdminRole),
    clientPublisherToken :: Maybe (Proxy 'PublisherRole),
    author :: Maybe Login
  }
  deriving (Eq, Show)

data Handle m = Handle
  { connectionString :: ConnectionString, 
    logger :: Handlers.Logger.Handle m,
    base :: Handlers.Database.Base.Handle m,
    user :: Handlers.Web.User.Handle m,
    client :: Client,
    getBody :: Request -> m B.ByteString,
    response404 :: Response,
    response200 :: Response,
    response403 :: Response,
    response400 :: Text -> Response,
    response500 :: Response,
    mkGoodResponse :: Builder -> Response,
    -- mkResponseForImage :: Image -> Response,
    response404WithImage :: Response
  }

data HandleImage m = HandleImage
  { 
    response400B :: Text -> Response,
    response500B :: Response,
    mkResponseForImage :: Image -> Response,
    pullImage :: NumberImage -> m (Either SomeException (Maybe Image)),
    loggerImage :: Handlers.Logger.Handle m
  }        

-- data HandleUser m = HandleUser
--   { 
--     response400U :: Text -> Response,
--     response500U :: Response,
--     response200U :: Response,
--     userOffsetU :: Int,
--     userLimitU :: Int,
--     mkGoodResponseU :: Builder -> Response,
--     pullAllUsers :: Offset -> Limit -> m (Either SomeException [User]),
--     getTime :: m UTCTime,
--     findUserByLogin :: Login -> m (Either SomeException (Maybe User)),
--     makeHashPassword :: PasswordUser -> UTCTime -> HashPasswordUser,
--     putUser :: UserInternal -> UTCTime -> m (Either SomeException Success),
--     getBodyU :: Request -> m B.ByteString,
--     loggerUser :: Handlers.Logger.Handle m
--   }        

    -- userOffset :: Int,
    -- userLimit :: Int,
-- createUserBase :: (Monad m) => Handle m -> UserInternal -> m (Either T.Text Success)
-- createUserBase h user@(UserInternal {..}) = do
--   let logHandle = logger h
--   tryFind <- findUserByLogin h loginUser
--   case tryFind of
--     Left e -> do
--       logMessage logHandle Error "function findUserByLogin fail"
--       pure . Left . T.pack . displayException $ e
--     Right (Just _) -> do
--       logMessage logHandle Warning ("Login arleady taken: " <> getLogin loginUser)
--       pure $ Left "Login arleady taken"
--     Right Nothing -> do
--       time <- getTime h
--       let pwd' = makeHashPassword h passwordUser time
--       tryCreate <- putUser h (user {passwordUser = MkPasswordUser pwd'}) time
--       when (isLeft tryCreate) (logMessage logHandle Handlers.Logger.Error "Can't putUser")
--       pure $ either (Left . T.pack . displayException) Right tryCreate
            -- Handlers.Database.Base.pullAllUsers = DA.pullAllUsers pginfo (cLimitData cfg),
-- endPointUsers :: (Monad m) => Handle m -> Request -> m Response
-- endPointUsers h req = do
--   let logHandle = logger h
--       baseHandle = base h
--   case rawPathInfo req of
--     "/users/create" -> do
--       case client h of
--         Client {clientAdminToken = (Just adminRole)} -> createUser adminRole h req
--         _ -> do
--           Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
--           pure $ response404 h
--     "/users" -> do
--       let queryLimit = queryString req
--           (userOffset, userLimit) = queryToPaginate queryLimit
--
--       let newBaseHandle = baseHandle {Handlers.Database.Base.userOffset = userOffset, Handlers.Database.Base.userLimit = userLimit}
--       existingUsers (h {base = newBaseHandle}) req
--     _ -> do
--       Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point Users not found"
--       pure $ response404 h
