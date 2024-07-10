module Handlers.WebLogic where

import Scheme (User(..), Image)
import Web.WebType (UserToWeb(..), UserFromWeb(..), CategoryFromWeb (..), EditCategoryFromWeb(..), NewsFromWeb(..), EditNewsFromWeb(..))
import Web.WebType (userToWeb, webToUser, categoryToWeb, webToCategory, webToEditCategory, webToNews, webToEditNews, newsToWeb)
import qualified Handlers.Logger
import qualified Handlers.Base
-- import Network.Wai (Request, Response, rawPathInfo, queryString, rawQueryString, responseBuilder)
import Network.Wai (Request, Response, rawPathInfo, queryString)
import qualified Data.ByteString as B 
import Data.ByteString.Char8 as BC (readInt, empty, null)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L 
import Network.HTTP.Types (notFound404, status200, Query)
-- import Network.HTTP.Types (notFound404, status200, status201, Status, ResponseHeaders)
import qualified Data.Text.Encoding as E
import Data.Binary.Builder(Builder(..))

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
   -- logMessage 
    base :: Handlers.Base.Handle m,
    -- updateNews, createNews, updateCategory, createGategory , createUser
    getBody :: Request -> m (B.ByteString),
    response404 :: Response,
    response200 :: Response,
    mkGoodResponse :: Builder -> Response,
    mkResponseForImage :: Image -> Response
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
    path | B.isPrefixOf "/news" path  ->    endPointNews h req
         | B.isPrefixOf "/users" path  ->  endPointUsers h req 
         | B.isPrefixOf "/categories" path  -> endPointCategories h req
         | B.isPrefixOf "/images" path  ->  endPointImages h req
         | otherwise -> do
            Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"  
            pure (response404 h) -- todo. replace 404 for another error

endPointUsers :: (Monad m) => Handle m -> Request -> m (Response) 
endPointUsers h req = do
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "end Point Users"
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (E.decodeUtf8 $ rawPathInfo req)
  case rawPathInfo req of
    "/users/create" -> createUser h req -- создание пользователя tolko for admin todo
    "/users" -> do
      let queryLimit = queryString req
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryLimit)
      let (userOffset, userLimit) = offsetAndLimitFromQuery queryLimit (0, maxBound)
      let newBaseHandle = baseHandle {Handlers.Base.userOffset = userOffset, Handlers.Base.userLimit = userLimit} 
      existingUsers (h {base = newBaseHandle}) req
    _ -> do
           Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point Users not found"  
           pure (response404 h) -- todo. replace 404 for another error
    --
    --

  --
existingUsers :: (Monad m) => Handle m -> Request -> m (Response) -- for ALl
existingUsers h req = do
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Get All users"
  getUsers <- Handlers.Base.getAllUsers baseHandle --(baseHandle {Handlers.Base.userOffset = 1, Handlers.Base.userLimit = 2})
  case getUsers of
    Left e -> do
      Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e  
      pure $ response404 h -- "Not ok. 
    Right users -> pure . mkGoodResponse h . userToWeb $ users
  
createUser :: (Monad m) => Handle m -> Request -> m (Response) -- for Admin
-- createUser :: (Monad m) => Handle m -> Proxy Admin -> Request -> m (Response) -- for Admin
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
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Try to create user WEB"
      tryCreateUser <- Handlers.Base.createUserBase baseHandle name login password admin publisher
      case tryCreateUser of
        Right _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create User success WEB"
          pure (response200 h)
        Left e -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e  
          pure $ response404 h -- "Not ok. 

endPointImages :: (Monad m) => Handle m -> Request -> m (Response) 
endPointImages h req = do
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "end Point Images"
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (E.decodeUtf8 $ rawPathInfo req)
  case rawPathInfo req of
    "/images" -> existingImages h req -- получение одной картинки 
    _ -> do
           Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"  
           pure (response404 h)

existingImages :: (Monad m) => Handle m -> Request -> m (Response)
existingImages h req = do
  let logHandle = logger h 
  let baseHandle = base h 
  let queryImage = queryString req
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Give image with query string"
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryImage)
  -- Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (E.decodeUtf8 $ rawQueryString req) 
  case queryImage of
    [("id", Just n)] | idImage > 0 -> do -- todo maxBound int64 and maxBound (fst . readInt n) ?
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Good request image"  
      eImage <- Handlers.Base.getImage baseHandle idImage
      pure $ case eImage of
        Left e -> response404 h
        Right img -> mkResponseForImage h img
      where idImage = maybe (-1) (fromIntegral . fst) (BC.readInt n)  
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Bad request image"  
      pure (response404 h)

endPointCategories :: (Monad m) => Handle m -> Request -> m (Response) 
endPointCategories h req = do
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "end Point Categories"
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (E.decodeUtf8 $ rawPathInfo req)
  case rawPathInfo req of
    "/categories/create"-> createCategory h req -- создание категории 
    "/categories/edit" -> updateCategory h req -- редактирование категории (названия и смена родительской)
    "/categories" -> do
      let queryLimit = queryString req
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryLimit)
      let (userOffset, userLimit) = offsetAndLimitFromQuery queryLimit (0, maxBound)
      let newBaseHandle = baseHandle {Handlers.Base.userOffset = userOffset, Handlers.Base.userLimit = userLimit} 
      existingCategories (h {base = newBaseHandle}) req  
    _ -> do
           Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point not found"  
           pure $ response404 h

existingCategories :: (Monad m) => Handle m -> Request -> m (Response)
existingCategories h req = do 
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Get All categories Web"
  categories <- Handlers.Base.getAllCategories (baseHandle {Handlers.Base.userOffset = 1, Handlers.Base.userLimit = 2})
  --proveryaj
  case categories of
    Left e -> do
      Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e  
      pure $ response404 h -- "Not ok. 
    Right c -> pure . mkGoodResponse h . categoryToWeb $ c

createCategory :: (Monad m) => Handle m -> Request -> m (Response)
createCategory h req = do 
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create Category WEB"
  body <- webToCategory <$> getBody h req -- :: (Either String CategoryFromWeb)
  case body of
    Left e -> do 
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode Category WEB"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)  
      pure (response404 h) -- "Not ok. 
    Right (CategoryFromWeb label parent) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "try create category"
      tryCreateCategory <- Handlers.Base.createCategoryBase baseHandle label parent
      case tryCreateCategory of
        Right _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create Category success WEB"
          pure $ response200 h
        Left e -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e  
          pure $ response404 h -- "Not ok. 

updateCategory :: (Monad m) => Handle m -> Request -> m (Response)
--todo move from Right Right to LVL DataBase edit category New label
updateCategory h req = do 
  let logHandle = logger h 
  let baseHandle = base h 
  -- let queryEditCategory = queryString req
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit Category WEB"
  -- Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryEditCategory)
  body <- webToEditCategory <$> getBody h req -- :: (Either String EditCategoryFromWeb)
  case body of
    Left e -> do 
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode Edit Category WEB"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)  
      pure (response404 h) -- "Not ok. 
    Right (EditCategoryFromWeb label (Just newlabel) newparent) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "try edit category Just newlabel parent"
      tryEditCategory <- Handlers.Base.updateCategory baseHandle label newlabel newparent
      case tryEditCategory of
        Right _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit Category success WEB"
          pure $ response200 h
        Left _ -> pure $ response404 h -- "Not ok. 
    Right (EditCategoryFromWeb label Nothing newparent) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "try edit category without new label"
      tryEditCategory <- Handlers.Base.updateCategory baseHandle label label newparent
      case tryEditCategory of
        Right _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit Category success WEB"
          pure $ response200 h
        Left _ -> pure $ response404 h -- "Not ok. 

endPointNews :: (Monad m) => Handle m -> Request -> m (Response) 
endPointNews h req = do
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "end Point News"
  -- Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (T.pack $ show $ B.unpack $ rawPathInfo req)
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (E.decodeUtf8 $ rawPathInfo req)
  case rawPathInfo req of
    "/news/create" -> createNews h req -- создание новости
    "/news/edit" -> updateNews h req -- редактирование новости
    "/news" -> do
      let queryLimit = queryString req
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryLimit)
      let (userOffset, userLimit) = offsetAndLimitFromQuery queryLimit (0, maxBound)
      let newBaseHandle = baseHandle {Handlers.Base.userOffset = userOffset, Handlers.Base.userLimit = userLimit} 
      existingNews (h {base = newBaseHandle}) req
    _ -> do
           Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point not found"  
           pure $ response404 h 
    -- _ -> error "rawPathInfo req /= /news"
    --
createNews :: (Monad m) => Handle m -> Request -> m (Response)
createNews h req = do 
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create News WEB"
  body <- webToNews <$> getBody h req -- :: (Either String NewsFromWeb)
  case body of
    Left e -> do 
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode News WEB"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)  
      pure (response404 h) -- "Not ok. 
-- data NewsFromWeb = NewsFromWeb {title :: T.Text, login :: T.Text, label :: T.Text, content :: T.Text,
    Right (NewsFromWeb title login label content images publish) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "try create news"
      tryCreateNews <- Handlers.Base.createNewsBase baseHandle title login label content images publish
      case tryCreateNews of
        Right _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create News success WEB"
          pure $ response200 h
        Left e -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e  
          pure $ response404 h -- "Not ok. 
 
updateNews :: (Monad m) => Handle m -> Request -> m (Response)
updateNews h req = do 
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit News WEB"
  body <- webToEditNews <$> getBody h req -- :: (Either String EditNewsFromWeb)
  case body of
    Left e -> do 
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode Edit News WEB"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)  
      pure (response404 h) -- "Not ok. 
    Right (EditNewsFromWeb title newTitle newLogin newLabel newContent newImages newIsPublish) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "try edit news "
      tryEditNews <- Handlers.Base.updateNews baseHandle 
        title newTitle newLogin
        newLabel newContent (maybe [] id newImages) newIsPublish
      case tryEditNews of
        Right _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit News success WEB"
          pure $ response200 h
        Left _ -> pure $ response404 h -- "Not ok. 

existingNews :: (Monad m) => Handle m -> Request -> m (Response)
existingNews h req = do 
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Get All news Web"
  news <- Handlers.Base.getAllNews (baseHandle {Handlers.Base.userOffset = 1, Handlers.Base.userLimit = 2})
  case news of
    Left e -> do
      Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e  
      pure $ response404 h -- "Not ok. 
    Right news' -> pure . mkGoodResponse h . newsToWeb $ news' 

offsetAndLimitFromQuery :: Query -> (Int, Int) -> (Int, Int)
offsetAndLimitFromQuery [] (o , l) = (o,l)
offsetAndLimitFromQuery (("limit", Just limit) : xs) (o , l) = case (fmap . fmap) BC.null (BC.readInt limit) of 
                                                Just (l',True) -> offsetAndLimitFromQuery xs (o,l')
                                                _ -> offsetAndLimitFromQuery xs (o, l)
offsetAndLimitFromQuery (("offset", Just offset) : xs) (o , l) = case (fmap . fmap) BC.null (BC.readInt offset) of 
                                                Just (o',True) -> offsetAndLimitFromQuery xs (o',l)
                                                _ -> offsetAndLimitFromQuery xs (o, l)
