module Handlers.Web.Category.Get (existingCategories) where

import Handlers.Database.Api (getAllCategories)
import qualified Handlers.Logger
import Handlers.Web.Category (Handle (..))
import Network.Wai (Request, Response)
import Web.DTO.Category (categoryToWeb)
import qualified Web.Utils as WU

existingCategories :: (Monad m) => Handle m -> Request -> m Response
existingCategories h _req = do
  let logHandle = logger h
      baseHandle = base h
  categories <- getAllCategories baseHandle
  case categories of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error e
      pure $ WU.response500
    Right c -> pure . WU.mkGoodResponse . categoryToWeb $ c
