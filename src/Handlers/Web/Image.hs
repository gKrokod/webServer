module Handlers.Web.Image (Handle (..)) where

import Data.Binary.Builder (Builder)
import Data.Text (Text)
import qualified Handlers.Logger
import Network.Wai (Response)
import qualified Handlers.Database.Image
import Schema (Image (..))


data Handle m = Handle
  { 
    logger :: Handlers.Logger.Handle m,
    base :: Handlers.Database.Image.Handle m,
    response400 :: Text -> Response,
    response404 :: Response,
    response500 :: Response,
    mkResponseForImage :: Image -> Response,
    mkGoodResponse :: Builder -> Response
  }
