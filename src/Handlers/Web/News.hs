
module Handlers.Web.News (Handle (..)) where

import Data.Binary.Builder (Builder)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Handlers.Logger
import Network.Wai (Request, Response)
import qualified Handlers.Database.News
import qualified Handlers.Database.Auth

data Handle m = Handle
  { 
    logger :: Handlers.Logger.Handle m,
    base :: Handlers.Database.News.Handle m,
    auth :: Handlers.Database.Auth.Handle m,
    client :: Handlers.Database.Auth.Client,
    getBody :: Request -> m B.ByteString,
    response400 :: Text -> Response,
    response200 :: Response,
    response500 :: Response,
    response403 :: Response,
    response404 :: Response,
    mkGoodResponse :: Builder -> Response
  }
