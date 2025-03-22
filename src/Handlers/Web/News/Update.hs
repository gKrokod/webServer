{-# LANGUAGE RecordWildCards #-}

module Handlers.Web.News.Update (updateNews) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Handlers.Database.Api (getCopyRight, updateNewsBase)
import qualified Handlers.Logger
import Handlers.Web.News (Handle (..))
import Handlers.Web.News.Types (NewsEditInternal (..))
import Network.Wai (Request, Response)
import Schema (IsValidPassword (..))
import Types (Content (..), Label (..), Login (..), Title (..))
import Web.DTO.News (EditNewsFromWeb (..), webToEditNews)
import qualified Web.Utils as WU

type Author = Login

updateNews :: (Monad m) => Author -> Handle m -> Request -> m Response
updateNews author_ h req = do
  let logHandle = logger h
      authHandle = auth h
      baseHandle = base h
  body <- webToEditNews <$> getBody h req
  case body of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error (T.pack e)
      pure (WU.response400 . T.pack $ e)
    Right (EditNewsFromWeb {..}) -> do
      checkCopyright <- getCopyRight authHandle author_ (MkTitle title)
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
              pure WU.response200
            _ -> pure WU.response500
        Right NotValid ->
          pure WU.response403
        _ -> pure WU.response500
