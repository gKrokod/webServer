module Handlers.Web.Image.ImageApi (endPointImages) where

import qualified Handlers.Logger
import Handlers.Web.Base (Handle (..))
import Handlers.Web.Image.Get (existingImages)
import Network.Wai (Request, Response, rawPathInfo)

endPointImages :: (Monad m) => Handle m -> Request -> m Response
endPointImages h req = do
  case rawPathInfo req of
    "/images" -> existingImages h req
    _ -> do
      Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"
      pure $ response404 h
