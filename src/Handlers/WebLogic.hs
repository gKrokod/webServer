{-# LANGUAGE DataKinds       #-}
module Handlers.WebLogic where

import Scheme (Image, IsValidPassword(..), FilterItem (FilterPublishOrAuthor))
import Web.WebType (UserFromWeb(..), CategoryFromWeb (..), EditCategoryFromWeb(..), NewsFromWeb(..), EditNewsFromWeb(..))
import Web.WebType (userToWeb, webToUser, categoryToWeb, webToCategory, webToEditCategory, webToNews, webToEditNews, newsToWeb, queryToPanigate, queryToSort, queryToFind, queryToFilters, headersToLoginAndPassword)
import qualified Handlers.Logger
import qualified Handlers.Base
-- import Network.Wai (Request, Response, rawPathInfo, queryString, rawQueryString, responseBuilder)
import Network.Wai (Request, Response, rawPathInfo, queryString, requestHeaders)
import qualified Data.ByteString as B 
import Data.ByteString.Char8 as BC (readInt)
import qualified Data.Text as T
-- import qualified Data.ByteString.Lazy as L 
import Network.HTTP.Types (Query)
-- import Network.HTTP.Types (notFound404, status200, status201, Status, ResponseHeaders)
import qualified Data.Text.Encoding as E
import Data.Binary.Builder(Builder)
import Data.Maybe
import Control.Monad (when)
-- import Network.HTTP.Types.Header (RequestHeaders)
-- import Control.Exception (SomeException, displayException)
import Data.Proxy
import Data.Bool
import Control.Monad.Except
import Control.Monad.Trans (lift)

type Login = T.Text
type Author = T.Text

data ClientRole = AdminRole | PublisherRole 
  deriving (Eq, Show)

data Client = Client { clienAdminToken :: Maybe (Proxy 'AdminRole),
                       clientPublisherToken :: Maybe (Proxy 'PublisherRole),
                       author :: Maybe Login}
  deriving (Eq, Show)

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
   -- logMessage 
    base :: Handlers.Base.Handle m,
    client :: Client,
    getBody :: Request -> m (B.ByteString),
    response404 :: Response,
    response200 :: Response,
    mkGoodResponse :: Builder -> Response,
    mkResponseForImage :: Image -> Response,
    response404WithImage :: Response
  }
--
getClient :: (Monad m) => Handle m -> Request -> m (Either T.Text Client)
getClient h req = do
  let logHandle = logger h 
  let baseHandle = base h 
  let secureData = headersToLoginAndPassword . requestHeaders $ req
  when (isNothing secureData) (Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "Request don't have Login and Password")
  runExceptT $ do
    case secureData of
      Nothing -> pure $ Client Nothing Nothing Nothing
      Just (login, pass) -> do --
        (isAdmin, isPublisher) <- ExceptT $ Handlers.Base.getPrivilege baseHandle login 
        valid <- ExceptT $ Handlers.Base.getResultValid baseHandle login pass
        case valid of 
          NotValid -> do 
            lift $ Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "Password is incorrect"
            pure $ Client Nothing Nothing Nothing
          Valid -> do
            lift $ Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Password is correct"
            lift $ Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "get privilege" 
            pure $ Client (bool Nothing (Just Proxy) isAdmin) 
                          (bool Nothing (Just Proxy) isPublisher) 
                          (Just login)


doAutorization :: (Monad m) => Handle m -> Request -> m (Either Response (Handle m))
doAutorization h req = do
  let logHandle = logger h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Hello Autorization"  
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "get request"  
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show req)
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "get headers"  
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show $ requestHeaders req)
  userRole <- getClient h req
  case userRole of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error e 
      pure (Left $ response404 h) 
    Right clientRole -> do
      let h' = h {client = clientRole}
      pure $ Right h'

doLogic :: (Monad m) => Handle m -> Request -> m (Response) 
doLogic h req = do
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "run doLogic"  
  case rawPathInfo req of
    path | B.isPrefixOf "/news" path  ->    endPointNews h req
         | B.isPrefixOf "/users" path  ->  endPointUsers h req 
         | B.isPrefixOf "/categories" path  -> endPointCategories h req
         | B.isPrefixOf "/images" path  ->  endPointImages h req
         | otherwise -> do
            Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"  
            pure (response404 h) -- todo. replace 404 for another error
            -- pure (response404WithImage h) -- todo. replace 404 for another error

endPointUsers :: (Monad m) => Handle m -> Request -> m (Response) 
endPointUsers h req = do
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "end Point Users"
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (E.decodeUtf8 $ rawPathInfo req)
  case rawPathInfo req of
    "/users/create" -> do 
      case (client h) of
        Client (Just adminRole) _ _ -> createUser adminRole h req -- создание пользователя tolko for admin todo
        _ -> do
           Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"  
           pure (response404 h) -- todo. replace 404 for another error
    "/users" -> do
      let queryLimit = queryString req
      let (userOffset, userLimit) = queryToPanigate  queryLimit
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug ("Query String:")
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryLimit )
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show $ (userOffset, userLimit))

      -- let (userOffset, userLimit) = offsetAndLimitFromQuery queryLimit (0, maxBound)
      -- Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "show result Panigate and another"
      -- Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryLimit)
      -- Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show $ (no, nl))
      -- Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "show steps"
      -- let q2 = q1 queryLimit 
      -- Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show q2 )

      let newBaseHandle = baseHandle {Handlers.Base.userOffset = userOffset, Handlers.Base.userLimit = userLimit} 
      existingUsers (h {base = newBaseHandle}) req
    _ -> do
           Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point Users not found"  
           pure (response404 h) -- todo. replace 404 for another error
    --
    --

  -- Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create Category WEB"
  -- body <- webToCategory <$> getBody h req -- :: (Either String CategoryFromWeb)
  --
existingUsers :: (Monad m) => Handle m -> Request -> m (Response) -- for ALl
existingUsers h _req = do
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Get All users"
  getUsers <- Handlers.Base.getAllUsers baseHandle 
  case getUsers of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error e  
      pure $ response404 h -- "Not ok. 
    Right users -> pure . mkGoodResponse h . userToWeb $ users
  
createUser :: (Monad m) => Proxy 'AdminRole -> Handle m -> Request -> m (Response) -- for Admin
-- createUser :: (Monad m) => Handle m -> Proxy Admin -> Request -> m (Response) -- for Admin
createUser _ h req = do
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
      --todo new password?
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
      case eImage of
        Left e -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e  
          pure $ response404 h
        Right img -> pure $ mkResponseForImage h img
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
    "/categories/create"-> do
      case (client h) of
        Client (Just adminRole) _ _ -> createCategory adminRole h req -- создание категории 
        _ -> do
           Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"  
           pure (response404 h) -- todo. replace 404 for another error
    "/categories/edit" -> do
      case (client h) of
        Client (Just adminRole) _  _-> updateCategory adminRole h req -- редактирование категории (названия и смена родительской)
        _ -> do
           Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"  
           pure (response404 h) -- todo. replace 404 for another error
    "/categories" -> do
      let queryLimit = queryString req
      let (userOffset, userLimit) = queryToPanigate  queryLimit
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug ("Query String:")
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryLimit )
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show $ (userOffset, userLimit))

      -- Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "show steps"
      -- let q2 = q1 queryLimit 
      -- Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show q2 )
      -- let queryLimit = queryString req
      -- Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryLimit)
      -- let (userOffset, userLimit) = offsetAndLimitFromQuery queryLimit (0, maxBound)
      let newBaseHandle = baseHandle {Handlers.Base.userOffset = userOffset, Handlers.Base.userLimit = userLimit} 
      existingCategories (h {base = newBaseHandle}) req  
    _ -> do
           Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point not found"  
           pure $ response404 h

existingCategories :: (Monad m) => Handle m -> Request -> m (Response)
existingCategories h _req = do 
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Get All categories Web"
  categories <- Handlers.Base.getAllCategories baseHandle
  --proveryaj
  case categories of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error e  
      pure $ response404 h -- "Not ok. 
    Right c -> pure . mkGoodResponse h . categoryToWeb $ c

createCategory :: (Monad m) => Proxy 'AdminRole -> Handle m -> Request -> m (Response)
createCategory _ h req = do 
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

updateCategory :: (Monad m) => Proxy 'AdminRole -> Handle m -> Request -> m (Response)
--todo move from Right Right to LVL DataBase edit category New label
updateCategory _ h req = do 
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
  -- let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "end Point News"
  -- Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (T.pack $ show $ B.unpack $ rawPathInfo req)
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (E.decodeUtf8 $ rawPathInfo req)
  case rawPathInfo req of
    "/news/create" -> do 
      case (client h) of
        Client _ (Just publisherRole) _ -> createNews publisherRole h req -- создание новости
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"  
          pure (response404 h) -- todo. replace 404 for another error
    "/news/edit" -> 
      case (client h) of
        Client _ _ (Just author) -> do 
          updateNews author h req -- редактирование новости
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"  
          pure (response404 h) -- todo. replace 404 for another error
    "/news" -> do -- get all news
      let queryLimit = queryString req
      let (userOffset, userLimit) = queryToPanigate  queryLimit
      let sortWeb = queryToSort  queryLimit
      let findWeb = queryToFind  queryLimit
      let filtersWeb = queryToFilters queryLimit
      -- Debug
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug ("Query String:")
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryLimit )
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show $ (userOffset, userLimit))
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show $ sortWeb) 
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show $ findWeb) 
      let filterPublishOrAuthor = FilterPublishOrAuthor (author $ client h)
      _ <- mapM (Handlers.Logger.logMessage logHandle Handlers.Logger.Debug . T.pack . show) 
        (filterPublishOrAuthor : filtersWeb)
      -- Debug
      -- existingNews (setFilters (setFind (setSort (setPanigate h queryLimit) queryLimit) queryLimit) queryLimit) req
      existingNews (foldSets queryLimit h [setFilters, setFind, setSort, setPanigate]) req
    _ -> do
           Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point not found"  
           pure $ response404 h 
  where 
    foldSets :: (Monad m) => Query -> Handle m -> [Handle m -> Query -> Handle m] ->  Handle m
    foldSets query = foldr (\set h' -> set h' query)

    setPanigate :: (Monad m) => Handle m -> Query -> Handle m
    setPanigate h' q = 
      let 
          baseHandle = base h' 
          (userOffset, userLimit) = queryToPanigate q 
          newBaseHandle = baseHandle {Handlers.Base.userOffset = userOffset, Handlers.Base.userLimit = userLimit} 
      in h' {base = newBaseHandle}

    setSort ::  (Monad m) => Handle m -> Query -> Handle m
    setSort h' q = 
      let 
          baseHandle = base h' 
          (userSortColumn, userSortOrder) = queryToSort q
          newBaseHandle = baseHandle {Handlers.Base.sortColumnNews = userSortColumn, Handlers.Base.sortOrderNews = userSortOrder} 
      in h' {base = newBaseHandle}
          
    setFind ::  (Monad m) => Handle m -> Query -> Handle m
    setFind h' q = 
      let 
          baseHandle = base h' 
          mbFind = queryToFind q
          newBaseHandle = baseHandle {Handlers.Base.findSubString = mbFind }
      in h' {base = newBaseHandle}

    setFilters ::  (Monad m) => Handle m -> Query -> Handle m
    setFilters h' q = 
      let 
          baseHandle = base h' 
          filters = queryToFilters q
          filterVisible = FilterPublishOrAuthor (author $ client h) -- publish or author visible news
          newBaseHandle = baseHandle {Handlers.Base.filtersNews = (filterVisible : filters) }
      in h' {base = newBaseHandle}

createNews :: (Monad m) => Proxy 'PublisherRole -> Handle m -> Request -> m (Response)
createNews _ h req = do 
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

updateNews :: (Monad m) => Author -> Handle m -> Request -> m (Response)
updateNews author h req = do 
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
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Copyright check..."
      checkCopyright <- Handlers.Base.getCopyRight baseHandle author title
      case checkCopyright of
        Right Valid -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Copyright check: Ok"
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "try edit news "
          tryEditNews <- Handlers.Base.updateNews baseHandle 
            title newTitle newLogin
            newLabel newContent (maybe [] id newImages) newIsPublish
          case tryEditNews of
            Right _ -> do
              Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit News success WEB"
              pure $ response200 h
            _ -> pure $ response404 h -- "Not ok.  Left. delat log?
        Right NotValid -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Copyright check: Fail"
          pure $ response404 h
        _ -> pure $ response404 h -- "Not ok. 


existingNews :: (Monad m) => Handle m -> Request -> m (Response)
existingNews h _req = do 
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Get All news Web"
  news <- Handlers.Base.getAllNews baseHandle
  case news of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error e  
      pure $ response404 h -- "Not ok. 
    Right news' -> pure . mkGoodResponse h . newsToWeb $ news' 
