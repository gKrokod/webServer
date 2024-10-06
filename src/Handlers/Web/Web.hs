{-# LANGUAGE DataKinds #-}

module Handlers.Web.Web (Handle (..), Client (..), UserInternal (..), CategoryInternal (..), ClientRole (..), NewsInternal (..), NewsOut (..), NewsEditInternal (..)) where

import Data.Binary.Builder (Builder)
import qualified Data.ByteString as B
import Data.Proxy (Proxy (..))
import qualified Handlers.Base.Base
import qualified Handlers.Logger
import Handlers.Web.Category.Types (CategoryInternal (..))
import Handlers.Web.News.Types (NewsEditInternal (..), NewsInternal (..), NewsOut (..))
import Handlers.Web.User.Types (UserInternal (..))
import Network.Wai (Request, Response)
import Scheme (Image)
import Types (Login (..))

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
    base :: Handlers.Base.Base.Handle m,
    client :: Client,
    getBody :: Request -> m B.ByteString,
    response404 :: Response,
    response200 :: Response,
    mkGoodResponse :: Builder -> Response,
    mkResponseForImage :: Image -> Response,
    response404WithImage :: Response
  }
