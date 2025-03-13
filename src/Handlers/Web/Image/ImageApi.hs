module Handlers.Web.Image.ImageApi (endPointImages) where

import qualified Handlers.Web.Image

import Handlers.Logger (logMessage, Log (Warning))
import Handlers.Web.Base (Handle (..))
import Handlers.Web.Image.Get (existingImages)
import Network.Wai (Request, Response, rawPathInfo)

endPointImages :: (Monad m) => Handle m -> Request -> m Response
endPointImages h req = do
  let logHandle = Handlers.Web.Base.logger h
      imageHandle = Handlers.Web.Base.image h
  case rawPathInfo req of
    "/images" -> existingImages imageHandle req
    _ -> do
      logMessage logHandle  Warning "End point not found"
      pure $ Handlers.Web.Image.response404 imageHandle
