-- {-# LANGUAGE DataKinds #-}
module Handlers.Web.Image.ImageApi (endPointImages) where

import qualified Data.Text.Encoding as E
import qualified Handlers.Logger
import Handlers.Web.Image.Get.Api (existingImages)
import Handlers.Web.Web (Handle (..))
import Network.Wai (Request, Response, rawPathInfo)

endPointImages :: (Monad m) => Handle m -> Request -> m Response
endPointImages h req = do
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "end Point Images"
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (E.decodeUtf8 $ rawPathInfo req)
  case rawPathInfo req of
    "/images" -> existingImages h req -- get one image
    _ -> do
      Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"
      pure (response404 h)
