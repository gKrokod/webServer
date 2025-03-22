module Handlers.Web.News (Handle (..)) where

import qualified Data.ByteString as B
import qualified Handlers.Database.Auth
import qualified Handlers.Database.News
import qualified Handlers.Logger
import Network.Wai (Request)

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    base :: Handlers.Database.News.Handle m,
    auth :: Handlers.Database.Auth.Handle m,
    client :: Handlers.Database.Auth.Client,
    getBody :: Request -> m B.ByteString
  }
