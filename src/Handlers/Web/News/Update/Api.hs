module Handlers.Web.News.Update.Api (updateNews) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Handlers.Base.Api (getCopyRight, updateNewsBase)
import qualified Handlers.Logger
import Handlers.Web.News.Types (NewsEditInternal (..))
import Handlers.Web.Web (Handle (..))
import Network.Wai (Request, Response)
import Scheme (IsValidPassword (..))
import Types (Content (..), Label (..), Login (..), Title (..))
import Web.WebType (EditNewsFromWeb (..), webToEditNews)

type Author = Login

updateNews :: (Monad m) => Author -> Handle m -> Request -> m Response
updateNews author_ h req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit News WEB"
  body <- webToEditNews <$> getBody h req -- :: (Either String EditNewsFromWeb)
  case body of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode Edit News WEB"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)
      pure (response404 h) -- "Not ok.
    Right (EditNewsFromWeb title_ newTitle_ newLogin_ newLabel_ newContent_ newImages_ newIsPublish_) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Copyright check..."
      checkCopyright <- getCopyRight baseHandle author_ (MkTitle title_)
      case checkCopyright of
        Right Valid -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Copyright check: Ok"
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "try edit news "
          tryEditNews <-
            updateNewsBase
              baseHandle
              (MkTitle title_)
              ( NewsEditInternal
                  (fmap MkTitle newTitle_)
                  (fmap MkLogin newLogin_)
                  (fmap MkLabel newLabel_)
                  (fmap MkContent newContent_)
                  (fromMaybe [] newImages_)
                  newIsPublish_
              )
          case tryEditNews of
            Right _ -> do
              Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit News success WEB"
              pure $ response200 h
            _ -> pure $ response404 h -- "Not ok.
        Right NotValid -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Copyright check: Fail"
          pure $ response404 h
        _ -> pure $ response404 h -- "Not ok.
