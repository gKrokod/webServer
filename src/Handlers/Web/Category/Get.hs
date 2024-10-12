module Handlers.Web.Category.Get (existingCategories) where

import Handlers.Database.Api (getAllCategories)
import qualified Handlers.Logger
import Handlers.Web.Base (Handle (..))
import Network.Wai (Request, Response)
import Web.WebType (categoryToWeb)

existingCategories :: (Monad m) => Handle m -> Request -> m Response
existingCategories h _req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Get All categories Web"
  categories <- getAllCategories baseHandle
  case categories of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error e
      pure $ response404 h -- "Not ok.
    Right c -> pure . mkGoodResponse h . categoryToWeb $ c