module Handlers.Web.Image.Get (existingImages) where

import Data.ByteString.Char8 as BC (readInt)
import Handlers.Database.Api (getImage)
import qualified Handlers.Logger
import Handlers.Web.Image (Handle (..))
import Network.Wai (Request, Response, queryString)
import Types (NumberImage (..))
import qualified Web.Utils as WU

existingImages :: (Monad m) => Handle m -> Request -> m Response
existingImages h req = do
  let logHandle = logger h
      baseHandle = base h
      queryImage = queryString req
  case queryImage of
    [("id", Just n)] | idImage > 0 -> do
      eImage <- getImage baseHandle (MkNumberImage idImage)
      case eImage of
        Left e -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Error e
          pure WU.response500
        Right img -> pure $ WU.mkResponseForImage img
      where
        idImage = maybe (-1) (fromIntegral . fst) (BC.readInt n)
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error "Bad request image"
      pure $ WU.response400 "Bad request \n"
