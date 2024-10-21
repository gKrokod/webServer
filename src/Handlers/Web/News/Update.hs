{-# LANGUAGE RecordWildCards #-}

module Handlers.Web.News.Update (updateNews) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Handlers.Database.Api (getCopyRight, updateNewsBase)
import qualified Handlers.Logger
import Handlers.Web.Base (Handle (..))
import Handlers.Web.News.Types (NewsEditInternal (..))
import Network.Wai (Request, Response)
import Schema (IsValidPassword (..))
import Types (Content (..), Label (..), Login (..), Title (..))
import Web.WebType (EditNewsFromWeb (..), webToEditNews)

type Author = Login

updateNews :: (Monad m) => Author -> Handle m -> Request -> m Response
updateNews author_ h req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit News WEB"
  body <- webToEditNews <$> getBody h req
  case body of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode Edit News WEB"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)
      pure (response404 h)
    Right (EditNewsFromWeb {..}) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Copyright check..."
      checkCopyright <- getCopyRight baseHandle author_ (MkTitle title)
      case checkCopyright of
        Right Valid -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Copyright check: Ok"
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "try edit news "
          tryEditNews <-
            updateNewsBase
              baseHandle
              (MkTitle title)
              ( NewsEditInternal
                  { titleEditNews = fmap MkTitle newTitle,
                    authorEditNews = fmap MkLogin newLogin,
                    labelEditNews = fmap MkLabel newLabel,
                    contentEditNews = fmap MkContent newContent,
                    imagesEditNews = fromMaybe [] images,
                    isPublishEditNews = newIsPublish
                  }
              )
          case tryEditNews of
            Right _ -> do
              Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit News success WEB"
              pure $ response200 h
            _ -> pure $ response404 h
        Right NotValid -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Copyright check: Fail"
          pure $ response404 h
        _ -> pure $ response404 h
