{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.Web.News.Create (createNews) where

import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Handlers.Database.Api (createNewsBase)
import qualified Handlers.Logger
import Handlers.Web.Base (ClientRole (..), Handle (..))
import Handlers.Web.News.Types (NewsInternal (..))
import Network.Wai (Request, Response)
import Types (Content (..), Label (..), Login (..), Title (..))
import Web.WebType (NewsFromWeb (..), webToNews)

createNews :: (Monad m) => Proxy 'PublisherRole -> Handle m -> Request -> m Response
createNews _ h req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create News WEB"
  body <- webToNews <$> getBody h req
  case body of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode News WEB"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)
      pure (response404 h)
    Right (NewsFromWeb {..}) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "try create news"
      tryCreateNews <-
        createNewsBase
          baseHandle
          ( NewsInternal
              { titleNews = MkTitle title,
                authorNews = MkLogin login,
                labelNews = MkLabel label,
                contentNews = MkContent content,
                imagesNews = images,
                isPublishNews = isPublish
              }
          )
      case tryCreateNews of
        Right _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create News success WEB"
          pure $ response200 h
        Left e -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e
          pure $ response404 h
