{-# LANGUAGE DataKinds #-}

module Handlers.Web.Category.Update.Api (updateCategory) where

import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Handlers.Base.Api (updateCategoryBase)
import qualified Handlers.Logger
import Handlers.Web.Category.Types (CategoryInternal (..))
import Handlers.Web.Web (ClientRole (..), Handle (..))
import Network.Wai (Request, Response)
import Types (Label (..))
import Web.WebType (EditCategoryFromWeb (..), webToEditCategory)

updateCategory :: (Monad m) => Proxy 'AdminRole -> Handle m -> Request -> m Response
updateCategory _ h req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit Category WEB"
  body <- webToEditCategory <$> getBody h req -- :: (Either String EditCategoryFromWeb)
  case body of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode Edit Category WEB"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)
      pure (response404 h) -- "Not ok.
    Right (EditCategoryFromWeb label_ (Just newlabel_) newparent_) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "try edit category Just newlabel parent"
      tryEditCategory <-
        updateCategoryBase
          baseHandle
          (MkLabel label_)
          ( CategoryInternal
              { labelCategory = MkLabel newlabel_,
                parentCategory = fmap MkLabel newparent_
              }
          )
      case tryEditCategory of
        Right _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit Category success WEB"
          pure $ response200 h
        Left _ -> pure $ response404 h -- "Not ok.
    Right (EditCategoryFromWeb label_ Nothing newparent_) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "try edit category without new label"
      tryEditCategory <-
        updateCategoryBase
          baseHandle
          (MkLabel label_)
          ( CategoryInternal
              { labelCategory = MkLabel label_,
                parentCategory = fmap MkLabel newparent_
              }
          )
      case tryEditCategory of
        Right _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit Category success WEB"
          pure $ response200 h
        Left _ -> pure $ response404 h -- "Not ok.
