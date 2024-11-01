module Handlers.Web.News.Get (existingNews) where

import Handlers.Database.Api (getAllNews)
import qualified Handlers.Logger
import Handlers.Web.Base (Handle (..))
import Network.Wai (Request, Response)
import Web.WebType (newsToWeb)

existingNews :: (Monad m) => Handle m -> Request -> m Response
existingNews h _req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Get All news Web"
  news <- getAllNews baseHandle
  case news of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error e
      pure $ response500 h
    Right news' -> pure . mkGoodResponse h . newsToWeb $ news'
