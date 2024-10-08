{-# LANGUAGE DataKinds #-}

module Handlers.WebLogic (Handle (..), Client (..), doAuthorization, doLogic) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans (lift)
import Data.Binary.Builder (Builder)
import Data.Bool (bool)
import qualified Data.ByteString as B
import Data.ByteString.Char8 as BC (readInt)
import Data.Maybe (fromMaybe, isNothing)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Handlers.Base
import qualified Handlers.Logger
import Network.HTTP.Types (Query)
import Network.Wai (Request, Response, queryString, rawPathInfo, requestHeaders)
import Scheme (FilterItem (FilterPublishOrAuthor), Image, IsValidPassword (..))
import Types (CategoryInternal (..), Content (..), Label (..), Login (..), Name (..), NewsEditInternal (..), NewsInternal (..), NumberImage (..), PasswordUser (..), Title (..), UserInternal (..))
import Web.WebType (CategoryFromWeb (..), EditCategoryFromWeb (..), EditNewsFromWeb (..), NewsFromWeb (..), UserFromWeb (..), categoryToWeb, headersToLoginAndPassword, newsToWeb, queryToFilters, queryToFind, queryToPanigate, queryToSort, userToWeb, webToCategory, webToEditCategory, webToEditNews, webToNews, webToUser)

type Author = Login

data ClientRole = AdminRole | PublisherRole
  deriving (Eq, Show)

data Client = Client
  { clienAdminToken :: Maybe (Proxy 'AdminRole),
    clientPublisherToken :: Maybe (Proxy 'PublisherRole),
    author :: Maybe Login
  }
  deriving (Eq, Show)

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    -- logMessage
    base :: Handlers.Base.Handle m,
    client :: Client,
    getBody :: Request -> m B.ByteString,
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
      baseHandle = base h
      secureData = headersToLoginAndPassword . requestHeaders $ req
  when (isNothing secureData) (Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "Request don't have Login and Password")
  runExceptT $ do
    case secureData of
      Nothing -> pure $ Client Nothing Nothing Nothing
      Just (login_, password_) -> do
        --
        (isAdmin_, isPublisher_) <- ExceptT $ Handlers.Base.getPrivilege baseHandle (MkLogin login_)
        valid <- ExceptT $ Handlers.Base.getResultValid baseHandle (MkLogin login_) (MkPasswordUser password_)
        case valid of
          NotValid -> do
            lift $ Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "Password is incorrect"
            pure $ Client Nothing Nothing Nothing
          Valid -> do
            lift $ Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Password is correct"
            lift $ Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "get privilege"
            pure $
              Client
                (bool Nothing (Just Proxy) isAdmin_)
                (bool Nothing (Just Proxy) isPublisher_)
                (Just (MkLogin login_))

doAuthorization :: (Monad m) => Handle m -> Request -> m (Either Response (Handle m))
doAuthorization h req = do
  let logHandle = logger h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "do Authorization"
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

doLogic :: (Monad m) => Handle m -> Request -> m Response
doLogic h req = do
  let logHandle = logger h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "run doLogic"
  case rawPathInfo req of
    path
      | B.isPrefixOf "/news" path -> endPointNews h req
      | B.isPrefixOf "/users" path -> endPointUsers h req
      | B.isPrefixOf "/categories" path -> endPointCategories h req
      | B.isPrefixOf "/images" path -> endPointImages h req
      | otherwise -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point not found"
          pure (response404 h) -- todo. replace 404 for another error

endPointUsers :: (Monad m) => Handle m -> Request -> m Response
endPointUsers h req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "end Point Users"
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (E.decodeUtf8 $ rawPathInfo req)
  case rawPathInfo req of
    "/users/create" -> do
      case client h of
        Client (Just adminRole) _ _ -> createUser adminRole h req -- create User for only admin
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure (response404 h)
    "/users" -> do
      let queryLimit = queryString req
          (userOffset, userLimit) = queryToPanigate queryLimit
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Query String:"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryLimit)
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show (userOffset, userLimit))

      let newBaseHandle = baseHandle {Handlers.Base.userOffset = userOffset, Handlers.Base.userLimit = userLimit}
      existingUsers (h {base = newBaseHandle}) req
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point Users not found"
      pure (response404 h)

existingUsers :: (Monad m) => Handle m -> Request -> m Response -- for ALl
existingUsers h _req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Get All users"
  getUsers <- Handlers.Base.getAllUsers baseHandle
  case getUsers of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error e
      pure $ response404 h -- "Not ok.
    Right users -> pure . mkGoodResponse h . userToWeb $ users

createUser :: (Monad m) => Proxy 'AdminRole -> Handle m -> Request -> m Response -- for Admin
createUser _ h req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "create User WEB"
  body <- webToUser <$> getBody h req -- :: (Either String UserFromWeb)
  case body of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode User WEB"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)
      pure (response404 h)
    Right (UserFromWeb name_ login_ password_ admin_ publisher_) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Try to create user WEB"
      tryCreateUser <-
        Handlers.Base.createUserBase
          baseHandle
          ( UserInternal
              { nameUser = MkName name_,
                loginUser = MkLogin login_,
                passwordUser = MkPasswordUser password_,
                isAdminUser = admin_,
                isPublisherUser = publisher_
              }
          )
      case tryCreateUser of
        Right _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create User success WEB"
          pure (response200 h)
        Left e -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e
          pure $ response404 h -- "Not ok.

endPointImages :: (Monad m) => Handle m -> Request -> m Response
endPointImages h req = do
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "end Point Images"
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (E.decodeUtf8 $ rawPathInfo req)
  case rawPathInfo req of
    "/images" -> existingImages h req -- get one image
    _ -> do
      Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"
      pure (response404 h)

existingImages :: (Monad m) => Handle m -> Request -> m Response
existingImages h req = do
  let logHandle = logger h
      baseHandle = base h
      queryImage = queryString req
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Give image with query string"
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryImage)
  case queryImage of
    [("id", Just n)] | idImage > 0 -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Good request image"
      eImage <- Handlers.Base.getImage baseHandle (MkNumberImage idImage)
      case eImage of
        Left e -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e
          pure $ response404 h
        Right img -> pure $ mkResponseForImage h img
      where
        idImage = maybe (-1) (fromIntegral . fst) (BC.readInt n)
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Bad request image"
      pure (response404 h)

endPointCategories :: (Monad m) => Handle m -> Request -> m Response
endPointCategories h req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "end Point Categories"
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (E.decodeUtf8 $ rawPathInfo req)
  case rawPathInfo req of
    "/categories/create" -> do
      case client h of
        Client (Just adminRole) _ _ -> createCategory adminRole h req -- create a category for only admin
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure (response404 h) -- todo. replace 404 for another error
    "/categories/edit" -> do
      case client h of
        Client (Just adminRole) _ _ -> updateCategory adminRole h req -- edit a category for only admin
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure (response404 h)
    "/categories" -> do
      let queryLimit = queryString req
          (userOffset, userLimit) = queryToPanigate queryLimit
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Query String:"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryLimit)
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show (userOffset, userLimit))

      let newBaseHandle = baseHandle {Handlers.Base.userOffset = userOffset, Handlers.Base.userLimit = userLimit}
      existingCategories (h {base = newBaseHandle}) req
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point not found"
      pure $ response404 h

existingCategories :: (Monad m) => Handle m -> Request -> m Response
existingCategories h _req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Get All categories Web"
  categories <- Handlers.Base.getAllCategories baseHandle
  case categories of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error e
      pure $ response404 h -- "Not ok.
    Right c -> pure . mkGoodResponse h . categoryToWeb $ c

createCategory :: (Monad m) => Proxy 'AdminRole -> Handle m -> Request -> m Response
createCategory _ h req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create Category WEB"
  body <- webToCategory <$> getBody h req -- :: (Either String CategoryFromWeb)
  case body of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode Category WEB"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)
      pure (response404 h) -- "Not ok.
    Right (CategoryFromWeb label_ parent_) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "try create category"
      tryCreateCategory <-
        Handlers.Base.createCategoryBase
          baseHandle
          ( CategoryInternal
              { labelCategory = MkLabel label_,
                parentCategory = fmap MkLabel parent_
              }
          )
      case tryCreateCategory of
        Right _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create Category success WEB"
          pure $ response200 h
        Left e -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e
          pure $ response404 h -- "Not ok.

updateCategory :: (Monad m) => Proxy 'AdminRole -> Handle m -> Request -> m Response
updateCategory _ h req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit Category WEB"
  body <- webToEditCategory <$> getBody h req -- :: (Either String EditCategoryFromWeb)
  case body of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode Edit Category WEB"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)
      pure (response404 h) -- "Not ok.
    Right (EditCategoryFromWeb label_ (Just newlabel_) newparent_) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "try edit category Just newlabel parent"
      tryEditCategory <-
        Handlers.Base.updateCategoryBase
          baseHandle
          (MkLabel label_)
          ( CategoryInternal
              { labelCategory = MkLabel newlabel_,
                parentCategory = fmap MkLabel newparent_
              }
          )
      case tryEditCategory of
        Right _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit Category success WEB"
          pure $ response200 h
        Left _ -> pure $ response404 h -- "Not ok.
    Right (EditCategoryFromWeb label_ Nothing newparent_) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "try edit category without new label"
      tryEditCategory <-
        Handlers.Base.updateCategoryBase
          baseHandle
          (MkLabel label_)
          ( CategoryInternal
              { labelCategory = MkLabel label_,
                parentCategory = fmap MkLabel newparent_
              }
          )
      case tryEditCategory of
        Right _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit Category success WEB"
          pure $ response200 h
        Left _ -> pure $ response404 h -- "Not ok.

endPointNews :: (Monad m) => Handle m -> Request -> m Response
endPointNews h req = do
  let logHandle = logger h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "end Point News"
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (E.decodeUtf8 $ rawPathInfo req)
  case rawPathInfo req of
    "/news/create" -> do
      case client h of
        Client _ (Just publisherRole) _ -> createNews publisherRole h req -- create news for only publisher
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure (response404 h) --
    "/news/edit" ->
      case client h of
        Client _ _ (Just author_) -> do
          updateNews author_ h req -- edit news for only author
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure (response404 h) --
    "/news" -> do
      -- get all news
      let queryLimit = queryString req
          (userOffset, userLimit) = queryToPanigate queryLimit
          sortWeb = queryToSort queryLimit
          findWeb = queryToFind queryLimit
          filtersWeb = queryToFilters queryLimit
      -- Debug
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Query String:"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryLimit)
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show (userOffset, userLimit))
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show sortWeb)
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show findWeb)
      let filterPublishOrAuthor = FilterPublishOrAuthor (fmap getLogin $ author $ client h)
      mapM_
        (Handlers.Logger.logMessage logHandle Handlers.Logger.Debug . T.pack . show)
        (filterPublishOrAuthor : filtersWeb)
      existingNews (foldSets queryLimit h [setFilters, setFind, setSort, setPanigate]) req
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point not found"
      pure $ response404 h
  where
    foldSets :: (Monad m) => Query -> Handle m -> [Handle m -> Query -> Handle m] -> Handle m
    foldSets query = foldr (\set h' -> set h' query)

    setPanigate :: (Monad m) => Handle m -> Query -> Handle m
    setPanigate h' q =
      let baseHandle = base h'
          (userOffset, userLimit) = queryToPanigate q
          newBaseHandle = baseHandle {Handlers.Base.userOffset = userOffset, Handlers.Base.userLimit = userLimit}
       in h' {base = newBaseHandle}

    setSort :: (Monad m) => Handle m -> Query -> Handle m
    setSort h' q =
      let baseHandle = base h'
          (userSortColumn, userSortOrder) = queryToSort q
          newBaseHandle = baseHandle {Handlers.Base.sortColumnNews = userSortColumn, Handlers.Base.sortOrderNews = userSortOrder}
       in h' {base = newBaseHandle}

    setFind :: (Monad m) => Handle m -> Query -> Handle m
    setFind h' q =
      let baseHandle = base h'
          mbFind = queryToFind q
          newBaseHandle = baseHandle {Handlers.Base.findSubString = mbFind}
       in h' {base = newBaseHandle}

    setFilters :: (Monad m) => Handle m -> Query -> Handle m
    setFilters h' q =
      let baseHandle = base h'
          filters = queryToFilters q
          filterVisible = FilterPublishOrAuthor (fmap getLogin $ author $ client h) -- publish or author visible news
          newBaseHandle = baseHandle {Handlers.Base.filtersNews = filterVisible : filters}
       in h' {base = newBaseHandle}

createNews :: (Monad m) => Proxy 'PublisherRole -> Handle m -> Request -> m Response
createNews _ h req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create News WEB"
  body <- webToNews <$> getBody h req -- :: (Either String NewsFromWeb)
  case body of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode News WEB"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)
      pure (response404 h) -- "Not ok.
    Right (NewsFromWeb title_ login_ label_ content_ images_ publish_) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "try create news"
      tryCreateNews <-
        Handlers.Base.createNewsBase
          baseHandle
          ( NewsInternal
              { titleNews = MkTitle title_,
                authorNews = MkLogin login_,
                labelNews = MkLabel label_,
                contentNews = MkContent content_,
                imagesNews = images_,
                isPublishNews = publish_
              }
          )
      case tryCreateNews of
        Right _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create News success WEB"
          pure $ response200 h
        Left e -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e
          pure $ response404 h -- "Not ok.
          --

updateNews :: (Monad m) => Author -> Handle m -> Request -> m Response
updateNews author_ h req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit News WEB"
  body <- webToEditNews <$> getBody h req -- :: (Either String EditNewsFromWeb)
  case body of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode Edit News WEB"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)
      pure (response404 h) -- "Not ok.
    Right (EditNewsFromWeb title_ newTitle_ newLogin_ newLabel_ newContent_ newImages_ newIsPublish_) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Copyright check..."
      checkCopyright <- Handlers.Base.getCopyRight baseHandle author_ (MkTitle title_)
      case checkCopyright of
        Right Valid -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Copyright check: Ok"
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "try edit news "
          tryEditNews <-
            Handlers.Base.updateNews
              baseHandle
              (MkTitle title_)
              ( NewsEditInternal
                  (fmap MkTitle newTitle_)
                  (fmap MkLogin newLogin_)
                  (fmap MkLabel newLabel_)
                  (fmap MkContent newContent_)
                  (fromMaybe [] newImages_)
                  newIsPublish_
              )
          case tryEditNews of
            Right _ -> do
              Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit News success WEB"
              pure $ response200 h
            _ -> pure $ response404 h -- "Not ok.
        Right NotValid -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Copyright check: Fail"
          pure $ response404 h
        _ -> pure $ response404 h -- "Not ok.

existingNews :: (Monad m) => Handle m -> Request -> m Response
existingNews h _req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Get All news Web"
  news <- Handlers.Base.getAllNews baseHandle
  case news of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error e
      pure $ response404 h -- "Not ok.
    Right news' -> pure . mkGoodResponse h . newsToWeb $ news'
