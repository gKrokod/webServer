module Handlers.Web.News.NewsApi (endPointNews) where

import qualified Handlers.Database.Auth as A (Client (..))
import qualified Handlers.Logger
import Handlers.Web.Base (Handle (..))
import Handlers.Web.News.Create (createNews)
import Handlers.Web.News.Get (existingNews)
import Handlers.Web.News.Update (updateNews)
import Network.Wai (Request, Response, rawPathInfo)
import Schema (FilterItem (FilterPublishOrAuthor))
import Types (Login (..))
import qualified Web.Utils as WU

endPointNews :: (Monad m) => Handle m -> Request -> m Response
endPointNews h req = do
  let logHandle = Handlers.Web.Base.logger h
      newsHandle = Handlers.Web.Base.news h
      userRole = Handlers.Web.Base.client h
  case rawPathInfo req of
    "/news/create" -> do
      case userRole of
        A.Client {A.clientPublisherToken = (Just publisherRole)} -> createNews publisherRole newsHandle req
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure WU.response403
    "/news/edit" ->
      case userRole of
        A.Client {A.author = (Just author_)} -> do
          updateNews author_ newsHandle req
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure WU.response403
    "/news" -> do
      let filterAuthor = FilterPublishOrAuthor (fmap getLogin . A.author $ userRole)
      existingNews filterAuthor newsHandle req
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point not found"
      pure WU.response404
