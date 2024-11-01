module Handlers.Web.Image.Get (existingImages) where

import Data.ByteString.Char8 as BC (readInt)
import qualified Data.Text as T
import Handlers.Database.Api (getImage)
import qualified Handlers.Logger
import Handlers.Web.Base (Handle (..))
import Network.Wai (Request, Response, queryString)
import Types (NumberImage (..))

existingImages :: (Monad m) => Handle m -> Request -> m Response
existingImages h req = do
  let logHandle = logger h
      baseHandle = base h
      queryImage = queryString req
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Give image with query string"
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryImage)
  case queryImage of
    [("id", Just n)] | idImage > 0 -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Good request image"
      eImage <- getImage baseHandle (MkNumberImage idImage)
      case eImage of
        Left e -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e
          pure $ response500 h
        Right img -> pure $ mkResponseForImage h img
      where
        idImage = maybe (-1) (fromIntegral . fst) (BC.readInt n)
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error "Bad request image"
      pure $ response400 h "Bad request \n"
