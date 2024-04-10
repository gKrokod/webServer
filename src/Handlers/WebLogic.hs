module Handlers.WebLogic where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Handlers.Logger
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types (notFound404, status200, status201, Status, ResponseHeaders)
import Data.Binary.Builder as BR (fromByteString, Builder)
import Network.Wai (Request, Response, rawPathInfo)



data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    buildResponse :: Status -> ResponseHeaders -> Builder -> Response,
    paginate :: Int  -- limit from config file
  }

-- old  type Application = Request -> ResourceT IO Response
--
-- last type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived -- passing style?
-- type Application :: Request -> Respond -> IO ResponseReceived
-- type Respond = Response -> IO ResponseReceived

doLogic :: (Monad m) => Handle m -> Request -> m (Response) 
doLogic h req = do
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "get request"  
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
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (T.pack $ show $ B.unpack $ rawPathInfo req)
  case rawPathInfo req of
    path | B.isPrefixOf "/users/create" path  -> undefined -- создание пользователя 
         | B.isPrefixOf "/users" path  -> undefined -- получение списка всех 
         | otherwise -> do
            Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"  
            pure $ buildResponse h notFound404 [] "notFound bro\n"
    _ -> error "rawPathInfo req /= /users"


endPointNews :: (Monad m) => Handle m -> Request -> m (Response) 
endPointNews h req = do
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "end Point News"
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (T.pack $ show $ B.unpack $ rawPathInfo req)
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
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (T.pack $ show $ B.unpack $ rawPathInfo req)
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
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (T.pack $ show $ B.unpack $ rawPathInfo req)
  case rawPathInfo req of
    path | B.isPrefixOf "/images" path  -> undefined -- получение одной картинки 
         | otherwise -> do
            Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"  
            pure $ buildResponse h notFound404 [] "notFound bro\n"
    _ -> error "rawPathInfo req /= /images"
