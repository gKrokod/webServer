module Handlers.Web.News.Get (existingNews) where

import Handlers.Database.Api (getAllNews)
import qualified Handlers.Logger
import Handlers.Web.News (Handle (..))
import Network.Wai (Request, Response)
import Web.DTO.News (newsToWeb)
import qualified Web.Utils as WU

existingNews :: (Monad m) => Handle m -> Request -> m Response
existingNews h _req = do
  let logHandle = logger h
      baseHandle = base h
  news <- getAllNews baseHandle
  case news of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error e
      pure $ WU.response500
    Right news' -> pure . WU.mkGoodResponse . newsToWeb $ news'
