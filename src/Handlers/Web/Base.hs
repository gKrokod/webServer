{-# LANGUAGE DataKinds #-}

module Handlers.Web.Base (Handle (..), Client (..), UserInternal (..), CategoryInternal (..), ClientRole (..), NewsInternal (..), NewsOut (..), NewsEditInternal (..)) where

import Data.Binary.Builder (Builder)
import qualified Data.ByteString as B
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Handlers.Database.Base
import qualified Handlers.Logger
import Handlers.Web.Category.Types (CategoryInternal (..))
import Handlers.Web.News.Types (NewsEditInternal (..), NewsInternal (..), NewsOut (..))
import Handlers.Web.User.Types (UserInternal (..))
import Network.Wai (Request, Response)
import Schema (Image)
import Types (Login (..))

data ClientRole = AdminRole | PublisherRole
  deriving (Eq, Show)

data Client = Client
  { clientAdminToken :: Maybe (Proxy 'AdminRole),
    clientPublisherToken :: Maybe (Proxy 'PublisherRole),
    author :: Maybe Login
  }
  deriving (Eq, Show)

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    base :: Handlers.Database.Base.Handle m,
    client :: Client,
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
