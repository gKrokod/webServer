{-# LANGUAGE DataKinds #-}

module Handlers.Web.User (Handle (..)) where

import qualified Data.ByteString as B
import qualified Handlers.Database.User
import qualified Handlers.Logger
import Network.Wai (Request)

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    base :: Handlers.Database.User.Handle m,
    getBody :: Request -> m B.ByteString
  }
