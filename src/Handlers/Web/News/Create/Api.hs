{-# LANGUAGE DataKinds #-}

module Handlers.Web.News.Create.Api (createNews) where

import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Handlers.Base.Api (createNewsBase)
import qualified Handlers.Logger
import Handlers.Web.News.Types (NewsInternal (..))
import Handlers.Web.Web (ClientRole (..), Handle (..))
import Network.Wai (Request, Response)
import Types (Content (..), Label (..), Login (..), Title (..))
import Web.WebType (NewsFromWeb (..), webToNews)

createNews :: (Monad m) => Proxy 'PublisherRole -> Handle m -> Request -> m Response
createNews _ h req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create News WEB"
  body <- webToNews <$> getBody h req -- :: (Either String NewsFromWeb)
  case body of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode News WEB"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)
      pure (response404 h) -- "Not ok.
    Right (NewsFromWeb title_ login_ label_ content_ images_ publish_) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "try create news"
      tryCreateNews <-
        createNewsBase
          baseHandle
          ( NewsInternal
              { titleNews = MkTitle title_,
                authorNews = MkLogin login_,
                labelNews = MkLabel label_,
                contentNews = MkContent content_,
                imagesNews = images_,
                isPublishNews = publish_
              }
          )
      case tryCreateNews of
        Right _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create News success WEB"
          pure $ response200 h
        Left e -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e
          pure $ response404 h -- "Not ok.
          --
