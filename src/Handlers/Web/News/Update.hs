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
  body <- webToEditNews <$> getBody h req
  case body of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error (T.pack e)
      pure (response400 h . T.pack $ e)
    Right (EditNewsFromWeb {..}) -> do
      checkCopyright <- getCopyRight baseHandle author_ (MkTitle title)
      case checkCopyright of
        Right Valid -> do
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
            Right _ ->
              pure $ response200 h
            _ -> pure $ response500 h
        Right NotValid ->
          pure $ response403 h
        _ -> pure $ response500 h
