{-# LANGUAGE DataKinds #-}

module Handlers.Web.Category (Handle (..)) where

import qualified Data.ByteString as B
import qualified Handlers.Database.Category
import qualified Handlers.Logger
import Network.Wai (Request)

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    base :: Handlers.Database.Category.Handle m,
    getBody :: Request -> m B.ByteString
  }
