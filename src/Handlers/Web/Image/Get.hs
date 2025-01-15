module Handlers.Web.Image.Get (existingImages) where

import Data.ByteString.Char8 as BC (readInt)
import Handlers.Database.Api (getImage)
import qualified Handlers.Logger
import Handlers.Web.Base (HandleImage(..))
import Network.Wai (Request, Response, queryString)
import Types (NumberImage (..))

existingImages :: (Monad m) => HandleImage m -> Request -> m Response
existingImages h req = do
  let logHandle = loggerImage h
      queryImage = queryString req
  case queryImage of
    [("id", Just n)] | idImage > 0 -> do
      eImage <- getImage h (MkNumberImage idImage)
      case eImage of
        Left e -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Error e
          pure $ response500B h
        Right img -> pure $ mkResponseForImage h img
      where
        idImage = maybe (-1) (fromIntegral . fst) (BC.readInt n)
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error "Bad request image"
      pure $ response400B h "Bad request \n"
