
module Handlers.Web.News (Handle (..)) where

import Data.Binary.Builder (Builder)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Handlers.Logger
import Network.Wai (Request, Response)
import qualified Handlers.Database.News

data Handle m = Handle
  { 
    logger :: Handlers.Logger.Handle m,
    base :: Handlers.Database.News.Handle m,
    getBody :: Request -> m B.ByteString,
    response400 :: Text -> Response,
    response200 :: Response,
    response500 :: Response,
    response403 :: Response,
    response404 :: Response,
    mkGoodResponse :: Builder -> Response
  }
-- data Handle m = Handle
--   { connectionString :: ConnectionString, 
--     logger :: Handlers.Logger.Handle m,
--     base :: Handlers.Database.Base.Handle m,
--     user :: Handlers.Web.User.Handle m,
--     category :: Handlers.Web.Category.Handle m,
--     image :: Handlers.Web.Image.Handle m,
--     news :: Handlers.Web.News.Handle m,
--     client :: Client,
--     getBody :: Request -> m B.ByteString,
--     response404 :: Response,
--     response200 :: Response,
--     response403 :: Response,
--     response400 :: Text -> Response,
--     response500 :: Response,
--     mkGoodResponse :: Builder -> Response,
--     mkResponseForImage :: Image -> Response,
--     response404WithImage :: Response
--   }
