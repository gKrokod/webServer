-- {-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Handlers.Web.Base (Handle (..), UserInternal (..), CategoryInternal (..), NewsInternal (..), NewsOut (..), NewsEditInternal (..)) where

import Data.Binary.Builder (Builder)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Handlers.Database.Base
import qualified Handlers.Database.Auth (Handle (..), Client(..))
import qualified Handlers.Logger
import Handlers.Web.Category.Types (CategoryInternal (..))
import Handlers.Web.News.Types (NewsEditInternal (..), NewsInternal (..), NewsOut (..))
import Handlers.Web.User.Types (UserInternal (..))
import Network.Wai (Request, Response)
import Schema (Image)
import Database.Persist.Postgresql (ConnectionString)
import qualified Handlers.Web.User (Handle (..))
import qualified Handlers.Web.Category (Handle (..))
import qualified Handlers.Web.Image (Handle (..))
import qualified Handlers.Web.News (Handle (..))


-- data ClientRole = AdminRole | PublisherRole
--   deriving (Eq, Show)
--
-- data Client = Client
--   { clientAdminToken :: Maybe (Proxy 'AdminRole),
--     clientPublisherToken :: Maybe (Proxy 'PublisherRole),
--     author :: Maybe Login
--   }
--   deriving (Eq, Show)

data Handle m = Handle
  { connectionString :: ConnectionString, 
    logger :: Handlers.Logger.Handle m,
    base :: Handlers.Database.Base.Handle m,
    auth :: Handlers.Database.Auth.Handle m,
    user :: Handlers.Web.User.Handle m,
    category :: Handlers.Web.Category.Handle m,
    image :: Handlers.Web.Image.Handle m,
    news :: Handlers.Web.News.Handle m,
    client :: Handlers.Database.Auth.Client,
    getBody :: Request -> m B.ByteString,
    response404 :: Response,
    response200 :: Response,
    response403 :: Response,
    response400 :: Text -> Response,
    response500 :: Response,
    mkGoodResponse :: Builder -> Response,
    mkResponseForImage :: Image -> Response,
    response404WithImage :: Response
  }

