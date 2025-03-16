{-# LANGUAGE DataKinds #-}

module Handlers.Web.User (Handle (..)) where

import Data.Binary.Builder (Builder)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Handlers.Database.User
import qualified Handlers.Logger
import Network.Wai (Request, Response)

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    base :: Handlers.Database.User.Handle m,
    getBody :: Request -> m B.ByteString,
    response200 :: Response,
    response400 :: Text -> Response,
    response500 :: Response,
    response404 :: Response,
    mkGoodResponse :: Builder -> Response
  }
