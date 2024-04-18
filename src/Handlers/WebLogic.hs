module Handlers.WebLogic where
-- import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Handlers.Logger
import qualified Handlers.Base
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types (notFound404, status200, status201, Status, ResponseHeaders)
import Data.Binary.Builder as B (fromByteString, Builder, fromLazyByteString, putStringUtf8)
import qualified Data.ByteString as B 
import Data.ByteString.Char8 as BC (readInt)
import qualified Data.ByteString.Lazy as L 
import Network.Wai (Request, Response, rawPathInfo, getRequestBodyChunk, queryString, rawQueryString)
import Users
import Images
import News
import Category
import Data.Tree
import Data.Aeson (eitherDecode, eitherDecodeStrict, encode, ToJSON)
import Data.ByteString.Base64 as B64
import Data.List (sort)

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    base :: Handlers.Base.Handle m,
    buildResponse :: Status -> ResponseHeaders -> Builder -> Response,
    getBody :: Request -> m (B.ByteString),
    paginate :: Int  -- limit from config file
  }

-- old  type Application = Request -> ResourceT IO Response
--gt
-- last type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived -- passing style?
-- type Application :: Request -> Respond -> IO ResponseReceived
-- type Respond = Response -> IO ResponseReceived
--
mkJSON :: (ToJSON a) =>  [a] -> L.ByteString
mkJSON xs = mconcat ["{\"answer\":[",L.intercalate "," (Prelude.map encode xs),"]}"]

doLogic :: (Monad m) => Handle m -> Request -> m (Response) 
doLogic h req = do
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "get request"  
  -- Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (T.pack $ show req)
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (T.pack $ show req)
  -- Handlers.Logger.logMessage (Handlers.WebLogic.logger h) Handlers.Logger.Debug "send response"  
  case rawPathInfo req of
    path | B.isPrefixOf "/news" path  -> endPointNews h req
         | B.isPrefixOf "/users" path  ->  endPointUsers h req   -- +
         | B.isPrefixOf "/categories" path  -> endPointCategories h req -- +
         | B.isPrefixOf "/images" path  ->  endPointImages h req -- +
         | otherwise -> do
            Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"  
            pure $ buildResponse h notFound404 [] "notFound bro\n"
    _ -> error "rawPathInfo req /= path"
    -- otherwise -> pure $ buildResponse h status200 [(hContentType, "text/plain")] (BR.fromByteString $ (rawPathInfo req))

endPointNews :: (Monad m) => Handle m -> Request -> m (Response) 
endPointNews h req = do
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "end Point News"
  -- Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (T.pack $ show $ B.unpack $ rawPathInfo req)
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (E.decodeUtf8 $ rawPathInfo req)
  case rawPathInfo req of
    path | B.isPrefixOf "/news/create" path  -> createNews h req -- создание новости
         | B.isPrefixOf "/news/edit" path  -> editNews h req -- редактирование новости
         | B.isPrefixOf "/news" path  -> existingNews h req -- получение новости
         | otherwise -> do
            Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"  
            pure $ buildResponse h notFound404 [] "notFound bro\n"
    _ -> error "rawPathInfo req /= /news"

createNews :: (Monad m) => Handle m -> Request -> m (Response)
createNews h req = do
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "create News"
  body <- eitherDecodeStrict <$> getBody h req  -- :: m (Either String News)
  case (body :: Either String News) of
    Left e -> do 
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode News"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)  
      pure $  buildResponse h notFound404 [] "Not ok. News cannot be created. Status 404\n"
    Right news -> do
      Handlers.Base.updateNews baseHandle news
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Succesfully create news"
      pure $ buildResponse h status200 [] "All ok. News created. Status 200\n" 

existingNews :: (Monad m) => Handle m -> Request -> m (Response)
-- add some sort in queryString
existingNews h req = do
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Give news"
  news <- Handlers.Base.takeNews baseHandle
  let body = mkJSON news
  -- let body = mkJSON (Handlers.Base.bank baseHandle)
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show body)
  pure $ buildResponse h status200 [] ("All ok. News list:\n" <> B.fromLazyByteString body)
-- /news?sort_by=category
--     * /news?created_at=2018-05-21
--     * /news?created_until=2018-05-21
--     * /news?created_since=2018-05-2
-- loadNews =  eitherDecode <$> L.readFile "config/news1.cfg"
--
-- title указывает какую новость редактируем содержимым json который лежит в body
editNews :: (Monad m) => Handle m -> Request -> m (Response)
editNews h req = do 
  let logHandle = logger h 
  let baseHandle = base h 
  let queryEditNews = queryString req
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit News with query string"
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryEditNews)
  -- failResponse h
  case queryEditNews of
    [("title", Just title)]-> do
      let title' = E.decodeUtf8 title 
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (title')
      mbNews <- Handlers.Base.findNews baseHandle title'
      case mbNews of
        Nothing -> do
            Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "News for editing not found"  
            failResponse h
        Just oldNews -> do
            Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "News for editing found"  
      -- to do
      -- найти картинку в базе по title
      -- если картинка есть, то распарсить поля, которые есть и заменить в новости
      -- после сохранить картинку
            body <- eitherDecodeStrict <$> getBody h req  -- :: m (Either String News)
            -- Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show body)
            case (fmap unBoxNews body :: Either String News) of
              Left e -> do
                Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)
                failResponse h
              Right newNews -> do
                Handlers.Logger.logMessage logHandle Handlers.Logger.Debug ("Correct data for edit news")
                Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show newNews)
                Handlers.Base.updateNews baseHandle (mergeNews oldNews newNews)
                existingNews h req
            -- categories <- Handlers.Base.takeCategories baseHandle
            -- let categories' = CategoryDictionary 
            --                   $ renameRose name' newname' 
            --                   $ changeRose name' parent' 
            --                   $ categoryDictionaryTree categories
            -- Handlers.Base.updateCategories baseHandle categories'
            -- existingNews h req
    _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Bad request edit news"  
          failResponse h

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

-- mkJSON :: Users -> L.ByteString
-- mkJSON users = mconcat ["{\"users\":[",L.intercalate "," (Prelude.map encode users),"]}"]

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

endPointCategories :: (Monad m) => Handle m -> Request -> m (Response) 
endPointCategories h req = do
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "end Point Categories"
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (E.decodeUtf8 $ rawPathInfo req)
  case rawPathInfo req of
    path | B.isPrefixOf "/categories/create" path  -> createCategory h req -- создание категории 
         | B.isPrefixOf "/categories/edit" path  -> editCategory h req -- редактирование категории (названия и смена родительской)
         | B.isPrefixOf "/categories" path  -> existingCategories h req -- получение списка всех 
         | otherwise -> do
            Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"  
            pure $ buildResponse h notFound404 [] "notFound bro\n"
    _ -> error "rawPathInfo req /= /categories"
--
-- to do
--
existingCategories :: (Monad m) => Handle m -> Request -> m (Response)
existingCategories h req = do 
  let logHandle = logger h 
  let baseHandle = base h 
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Give categories"
  categories <- Handlers.Base.takeCategories baseHandle
  -- let body = mkJSON (mconcat $ levels $ categoryDictionaryTree categories)
  let body = mkJSON (sort $ flatten $ categoryDictionaryTree categories)
  pure $ buildResponse h status200 [] ("All ok. Categories list:\n" <> B.fromLazyByteString body)

 -- редактирование названия и смена родительской категории
-- curl "127.0.0.1:4221/categories/edit?name=Angel&parent=Abstract"
-- curl -v -X POST 127.0.0.1:4221/categories/create?parent=Abstract -H "Content-Type: application/json" -d '{"categoryDictionaryTree":"Node {rootLabel :: a, subForest :: [Tree a]}"'
-- подумать надо дораоткой, чтобы создавать категорию, присывая дерево в теле запроса. надо ли это?
createCategory :: (Monad m) => Handle m -> Request -> m (Response)
createCategory h req = do 
  let logHandle = logger h 
  let baseHandle = base h 
  let queryCreateCategory = queryString req
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create Category"
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryCreateCategory)
  case queryCreateCategory of
    [("name", Just name), ("parent", Just parent)] -> do
      let [name',parent'] = map E.decodeUtf8 [name, parent]
      categories <- Handlers.Base.takeCategories baseHandle
      let categories' = CategoryDictionary 
                        $ insertRose parent' (Node name' []) 
                        $ categoryDictionaryTree categories
      Handlers.Base.updateCategories baseHandle categories'
      existingCategories h req
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Bad request create category"  
      failResponse h

 -- редактирование названия и смена родительской категории
-- curl "127.0.0.1:4221/categories/edit?name=Witch&newname=Pitch&parent=Woman"
editCategory :: (Monad m) => Handle m -> Request -> m (Response)
editCategory h req = do 
  let logHandle = logger h 
  let baseHandle = base h 
  let queryEditCategory = queryString req
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit Category with query string"
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryEditCategory)
  -- failResponse h
  case queryEditCategory of
    [("name", Just name), ("newname", Just newname), ("parent", Just parent)] -> do
      let [name',newname',parent'] = map E.decodeUtf8 [name, newname, parent]
      categories <- Handlers.Base.takeCategories baseHandle
      let categories' = CategoryDictionary 
                        $ renameRose name' newname' 
                        $ changeRose name' parent' 
                        $ categoryDictionaryTree categories
      Handlers.Base.updateCategories baseHandle categories'
      existingCategories h req
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Bad request edit category"  
      failResponse h

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

existingImages :: (Monad m) => Handle m -> Request -> m (Response)
existingImages h req = do
  let logHandle = logger h 
  let baseHandle = base h 
  let queryImage = queryString req
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Give image with query string"
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryImage)
  -- Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (E.decodeUtf8 $ rawQueryString req) 
  case queryImage of
    [("id", Just n)] -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Good request image"  
      let idImage = maybe (-1) fst (BC.readInt n) 
      mbImage <- Handlers.Base.findImage baseHandle idImage 
      case mbImage of
        Nothing -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning ("Image with id = \"" <> (E.decodeUtf8 n) <> "\" not found in base")
          failResponse h
        Just img -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Image found in base"  
          let contentType = E.encodeUtf8 $ iHeader img 
          let content = B64.decodeBase64Lenient $ E.encodeUtf8 $ iBase64 img 
          pure $ buildResponse h status200 [(hContentType, contentType)] (B.fromByteString content)
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Bad request image"  
      failResponse h

okResponse :: (Monad m) => Handle m -> m (Response)
okResponse h = pure $ buildResponse h status200 [] "All ok. status 200\n" 

failResponse :: (Monad m) => Handle m -> m (Response)
failResponse h = pure $ buildResponse h notFound404 [] "Not ok. status 404\n" 
defaultResponse = \h ->  buildResponse h status200 [] "default Response\n"
