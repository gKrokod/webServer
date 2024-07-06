module Handlers.WebLogic where

import qualified Handlers.Logger
import qualified Handlers.Base
-- import Network.Wai (Request, Response, rawPathInfo, queryString, rawQueryString, responseBuilder)
import Network.Wai (Request, Response, rawPathInfo)
import qualified Data.ByteString as B 
import qualified Data.Text as T
import Network.HTTP.Types (notFound404, status200, status201, Status, ResponseHeaders)

-- import qualified Data.Text.Encoding as E
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
    base :: Handlers.Base.Handle m,
    getBody :: Request -> m (B.ByteString),
    response404 :: Response,
    response200 :: Response
  }

-- old  type Application = Request -> ResourceT IO Response
--gt
-- last type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived -- passing style?
-- type Application :: Request -> Respond -> IO ResponseReceived
-- type Respond = Response -> IO ResponseReceived
--
-- mkJSON :: (ToJSON a) =>  [a] -> L.ByteString
-- mkJSON xs = mconcat ["{\"answer\":[",L.intercalate "," (Prelude.map encode xs),"]}"]

doLogic :: (Monad m) => Handle m -> Request -> m (Response) 
doLogic h req = do
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "get request"  
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (T.pack $ show req)
  case rawPathInfo req of
    path | B.isPrefixOf "/news" path  ->       pure $ response200 h --endPointNews h req
         | B.isPrefixOf "/users" path  ->  error "users" --endPointUsers h req   -- +
         | B.isPrefixOf "/categories" path  -> error "categories" --endPointCategories h req -- +
         | B.isPrefixOf "/images" path  ->  error "images" --endPointImages h req -- +
         | otherwise -> do
            Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"  
            pure (response404 h)
    _ -> do
      Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "rawPathInfo req /= path"  
      pure (response404 h) -- todo this

-- defaultResponse = \h ->  responseBuilder status200 [] "default Response\n"
