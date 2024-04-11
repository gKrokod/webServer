module Handlers.WebLogic where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Handlers.Logger
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types (notFound404, status200, status201, Status, ResponseHeaders)
import Data.Binary.Builder as B (fromByteString, Builder)
import Data.ByteString as B 
import Network.Wai (Request, Response, rawPathInfo, getRequestBodyChunk)
import Users
import Data.Aeson (eitherDecode, eitherDecodeStrict)



data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    buildResponse :: Status -> ResponseHeaders -> Builder -> Response,
    getBody :: Request -> m (ByteString),
    paginate :: Int  -- limit from config file
  }

-- old  type Application = Request -> ResourceT IO Response
--gt
-- last type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived -- passing style?
-- type Application :: Request -> Respond -> IO ResponseReceived
-- type Respond = Response -> IO ResponseReceived

defaultResponse = \h ->  buildResponse h status200 [] "default Response\n"

doLogic :: (Monad m) => Handle m -> Request -> m (Response) 
doLogic h req = do
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "get request"  
  -- Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (T.pack $ show req)
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (T.pack $ show req)
  -- Handlers.Logger.logMessage (Handlers.WebLogic.logger h) Handlers.Logger.Debug "send response"  
  case rawPathInfo req of
    path | B.isPrefixOf "/news" path  -> endPointNews h req
         | B.isPrefixOf "/users" path  ->  endPointUsers h req
         | B.isPrefixOf "/categories" path  -> endPointCategories h req
         | B.isPrefixOf "/images" path  ->  endPointImages h req
         | otherwise -> do
            Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"  
            pure $ buildResponse h notFound404 [] "notFound bro\n"
    _ -> error "rawPathInfo req /= path"
    -- otherwise -> pure $ buildResponse h status200 [(hContentType, "text/plain")] (BR.fromByteString $ (rawPathInfo req))

endPointUsers :: (Monad m) => Handle m -> Request -> m (Response) 
endPointUsers h req = do
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "end Point Users"
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (E.decodeUtf8 $ rawPathInfo req)
  case rawPathInfo req of
    -- path | B.isPrefixOf "/users/create" path  -> (do makeUserFrom h req; pure $ defaultResponse h) -- создание пользователя 
    path | B.isPrefixOf "/users/create" path  -> (do makeUserFrom h req >>= resp h) -- создание пользователя 
         | B.isPrefixOf "/users" path  -> undefined -- получение списка всех 
         | otherwise -> do
            Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"  
            pure $ buildResponse h notFound404 [] "notFound bro\n"
    _ -> error "rawPathInfo req /= /users"

resp :: (Monad m) => Handle m -> User -> m (Response)
resp h user = do
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "resp User"
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (T.pack $ show user)
  pure $ buildResponse h status200 [] "User create\n"


makeUserFrom :: (Monad m) => Handle m -> Request -> m (User)
makeUserFrom h req = do
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "make User"
  body <- getBody h req
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (E.decodeUtf8 body)
  either (\_ -> pure $ MkUser "a" "b" "c " True True) (pure) (eitherDecodeStrict body)

endPointNews :: (Monad m) => Handle m -> Request -> m (Response) 
endPointNews h req = do
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "end Point News"
  -- Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (T.pack $ show $ B.unpack $ rawPathInfo req)
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (E.decodeUtf8 $ rawPathInfo req)
  case rawPathInfo req of
    path | B.isPrefixOf "/news/create" path  -> undefined -- создание новости
         | B.isPrefixOf "/news/edit" path  -> undefined -- редактирование новости
         | B.isPrefixOf "/news" path  -> undefined -- получение новости
         | otherwise -> do
            Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"  
            pure $ buildResponse h notFound404 [] "notFound bro\n"
    _ -> error "rawPathInfo req /= /news"

-- /news?sort_by=category
--     * /news?created_at=2018-05-21
--     * /news?created_until=2018-05-21
--     * /news?created_since=2018-05-2
endPointCategories :: (Monad m) => Handle m -> Request -> m (Response) 
endPointCategories h req = do
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "end Point Categories"
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (E.decodeUtf8 $ rawPathInfo req)
  case rawPathInfo req of
    path | B.isPrefixOf "/categories/create" path  -> undefined -- создание категории 
         | B.isPrefixOf "/categories/edit" path  -> undefined -- редактирование категории (названия и смена родительской)
         | B.isPrefixOf "/categories" path  -> undefined -- получение списка всех 
         | otherwise -> do
            Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"  
            pure $ buildResponse h notFound404 [] "notFound bro\n"
    _ -> error "rawPathInfo req /= /categories"


endPointImages :: (Monad m) => Handle m -> Request -> m (Response) 
endPointImages h req = do
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "end Point Images"
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (E.decodeUtf8 $ rawPathInfo req)
  case rawPathInfo req of
    path | B.isPrefixOf "/images" path  -> undefined -- получение одной картинки 
         | otherwise -> do
            Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"  
            pure $ buildResponse h notFound404 [] "notFound bro\n"
    _ -> error "rawPathInfo req /= /images"
