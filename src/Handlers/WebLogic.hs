module Handlers.WebLogic where

import Scheme (usersToBuilder)
import Scheme (User(..))
import Web.WebType (UserToWeb(..), UserFromWeb(..), userToWeb, webToUser)
import qualified Handlers.Logger
import qualified Handlers.Base
import qualified Handlers.Base
-- import Network.Wai (Request, Response, rawPathInfo, queryString, rawQueryString, responseBuilder)
import Network.Wai (Request, Response, rawPathInfo)
import qualified Data.ByteString as B 
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L 
import Network.HTTP.Types (notFound404, status200)
-- import Network.HTTP.Types (notFound404, status200, status201, Status, ResponseHeaders)
import qualified Data.Text.Encoding as E
import Data.Binary.Builder(Builder(..), fromLazyByteString)
-- import Data.Aeson (ToJSON, encode)
-- import Data.Binary.Builder as BU (fromByteString, Builder, fromLazyByteString, putStringUtf8)

-- import Network.HTTP.Types.Header (hContentType)
-- import Data.Binary.Builder as B (fromByteString, Builder, fromLazyByteString, putStringUtf8)
-- import Data.ByteString.Char8 as BC (readInt)
-- import qualified Data.ByteString.Lazy as L 
-- import Users
-- import Images
-- import News
-- import Category
-- import Data.Tree
-- import Data.Aeson (eitherDecode, eitherDecodeStrict, encode, ToJSON)
-- import Data.ByteString.Base64 as B64
-- import Data.List (sort)

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
   -- logMessage 
    base :: Handlers.Base.Handle m,
    -- updateNews, createNews, updateCategory, createGategory , createUser
    getBody :: Request -> m (B.ByteString),
    response404 :: Response,
    response200 :: Response,
    mkGoodResponse :: Builder -> Response
  }

-- old  type Application = Request -> ResourceT IO Response
--gt
-- last type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived -- passing style?
-- type Application :: Request -> Respond -> IO ResponseReceived
-- type Respond = Response -> IO ResponseReceived
--

doLogic :: (Monad m) => Handle m -> Request -> m (Response) 
doLogic h req = do
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "get request"  
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (T.pack $ show req)
  case rawPathInfo req of
    path | B.isPrefixOf "/news" path  ->       pure $ response200 h --endPointNews h req
         | B.isPrefixOf "/users" path  ->  endPointUsers h req --endPointUsers h req   -- +
         | B.isPrefixOf "/categories" path  -> pure $ response200 h--endPointCategories h req -- +
         | B.isPrefixOf "/images" path  ->  pure $ response200 h --endPointImages h req -- +
         | otherwise -> do
            Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"  
            pure (response404 h) -- todo. replace 404 for another error

endPointUsers :: (Monad m) => Handle m -> Request -> m (Response) 
endPointUsers h req = do
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "end Point Users"
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (E.decodeUtf8 $ rawPathInfo req)
  case rawPathInfo req of
    path | "/users/create" == path  -> createUser h req -- создание пользователя 
         | "/users" == path  -> existingUsers h req  -- получение списка всех 
         | otherwise -> do
            Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point Users not found"  
            pure (response404 h) -- todo. replace 404 for another error
    -- _ -> error "rawPathInfo req /= /users"
    --
existingUsers :: (Monad m) => Handle m -> Request -> m (Response)
existingUsers h req = do
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Get All users"
  mkGoodResponse h <$> userToWeb <$> Handlers.Base.getAllUsers baseHandle
  
createUser :: (Monad m) => Handle m -> Request -> m (Response)
createUser h req = do
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "create User WEB"
  body <- webToUser <$> getBody h req -- :: (Either String UserFromWeb)
  case body of
    Left e -> do 
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode User WEB"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)  
      pure (response404 h) -- "Not ok. User cannot be created. Status 404\n"
    Right (UserFromWeb name login password admin publisher) -> do
      tryCreateUser <- Handlers.Base.createUser baseHandle name login password admin publisher
      case tryCreateUser of
        Left e -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning e  
          pure (response404 h) -- "Not ok. User cannot be created. Status 404\n"
        Right _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create User success WEB"
          pure (response200 h)
