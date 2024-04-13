module Handlers.WebLogic where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Handlers.Logger
import qualified Handlers.Base
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types (notFound404, status200, status201, Status, ResponseHeaders)
import Data.Binary.Builder as B (fromByteString, Builder, fromLazyByteString, putStringUtf8)
import Data.ByteString as B 
import Data.ByteString.Char8 as BC (readInt)
import qualified Data.ByteString.Lazy as L 
import Network.Wai (Request, Response, rawPathInfo, getRequestBodyChunk, queryString, rawQueryString)
import Users
import Images
import Data.Aeson (eitherDecode, eitherDecodeStrict, encode)
import Data.ByteString.Base64 as B64


data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    base :: Handlers.Base.Handle m,
    buildResponse :: Status -> ResponseHeaders -> Builder -> Response,
    getBody :: Request -> m (ByteString),
    paginate :: Int  -- limit from config file
  }

-- old  type Application = Request -> ResourceT IO Response
--gt
-- last type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived -- passing style?
-- type Application :: Request -> Respond -> IO ResponseReceived
-- type Respond = Response -> IO ResponseReceived


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
    path | B.isPrefixOf "/users/create" path  -> createUser h req -- создание пользователя 
         | B.isPrefixOf "/users" path  -> existingUsers h req  -- получение списка всех 
         | otherwise -> do
            Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"  
            pure $ buildResponse h notFound404 [] "notFound bro\n"
    _ -> error "rawPathInfo req /= /users"

existingUsers :: (Monad m) => Handle m -> Request -> m (Response)
existingUsers h req = do
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Give users"
  users <- Handlers.Base.takeUsers baseHandle
  let body = mkJSON users
  -- let body = mkJSON (Handlers.Base.bank baseHandle)
  pure $ buildResponse h status200 [] ("All ok. User list:\n" <> B.fromLazyByteString body)

mkJSON :: Users -> L.ByteString
mkJSON users = mconcat ["{\"users\":[",L.intercalate "," (Prelude.map encode users),"]}"]

createUser :: (Monad m) => Handle m -> Request -> m (Response)
createUser h req = do
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "create User"
  body <- eitherDecodeStrict <$> getBody h req -- :: (Either String User)
  case body of
    Left e -> do 
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode User"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)  
      pure $  buildResponse h notFound404 [] "Not ok. User cannot be created. Status 404\n"
    Right user -> do
      Handlers.Base.updateUser baseHandle user
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Succesfully create user"
      pure $ buildResponse h status200 [] "All ok. User created. Status 200\n" 

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
    path | B.isPrefixOf "/images" path  -> existingImages h req -- получение одной картинки 
         | otherwise -> do
            Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"  
            pure $ buildResponse h notFound404 [] "notFound bro\n"
    _ -> error "rawPathInfo req /= /images"

-- decode and encode sdelaj methodami. Kak perevodit' nomer id is ByteString v Int prosto?
-- Pochemu querystring tolko 1 znachenie?
existingImages :: (Monad m) => Handle m -> Request -> m (Response)
existingImages h req = do
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Give image"
  let queryImage = queryString req
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (T.pack $ show queryImage)
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (E.decodeUtf8 $  rawQueryString req) 
  case queryImage of
    [("id", Just n)] -> do
      Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "Good request image"  
      mbImage <- Handlers.Base.findImage baseHandle (maybe 0 fst (BC.readInt n)) -- tyt vukinyt oshibky, chto ne good nomer
      case mbImage of
        Nothing -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "Image not found in base"  
          failResponse h
        Just img -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "Image found in base"  
          let contentType = E.encodeUtf8 $ iHeader img 
          let content = B64.decodeBase64Lenient $ E.encodeUtf8 $ iBase64 img 
          pure $ buildResponse h status200 [(hContentType, contentType)] (B.fromByteString content)
    _ -> do
      Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "Bad request image"  
      failResponse h

okResponse :: (Monad m) => Handle m -> m (Response)
okResponse h = pure $ buildResponse h status200 [] "All ok. status 200\n" 

failResponse :: (Monad m) => Handle m -> m (Response)
failResponse h = pure $ buildResponse h notFound404 [] "Not ok. status 404\n" 
defaultResponse = \h ->  buildResponse h status200 [] "default Response\n"
