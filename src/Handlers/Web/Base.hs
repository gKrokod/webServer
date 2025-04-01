{-# LANGUAGE DataKinds #-}

module Handlers.Web.Base (Handle (..), UserInternal (..), CategoryInternal (..), NewsInternal (..), NewsOut (..), NewsEditInternal (..)) where

import Database.Persist.Postgresql (ConnectionString)
import qualified Handlers.Database.Auth (Client (..), Handle (..))
import qualified Handlers.Logger
import qualified Handlers.Web.Category (Handle (..))
import Handlers.Web.Category.Types (CategoryInternal (..))
import qualified Handlers.Web.Image (Handle (..))
import qualified Handlers.Web.News (Handle (..))
import Handlers.Web.News.Types (NewsEditInternal (..), NewsInternal (..), NewsOut (..))
import qualified Handlers.Web.User (Handle (..))
import Handlers.Web.User.Types (UserInternal (..))

data Handle m = Handle
  { connectionString :: ConnectionString,
    logger :: Handlers.Logger.Handle m,
    auth :: Handlers.Database.Auth.Handle m,
    user :: Handlers.Web.User.Handle m,
    category :: Handlers.Web.Category.Handle m,
    image :: Handlers.Web.Image.Handle m,
    news :: Handlers.Web.News.Handle m,
    client :: Handlers.Database.Auth.Client
  }
