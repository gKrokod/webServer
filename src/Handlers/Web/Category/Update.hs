{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.Web.Category.Update (updateCategory) where

import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Handlers.Database.Api (updateCategoryBase)
import qualified Handlers.Logger
import Handlers.Web.Base (ClientRole (..), Handle (..))
import Handlers.Web.Category.Types (CategoryInternal (..))
import Network.Wai (Request, Response)
import Types (Label (..))
import Web.WebType (EditCategoryFromWeb (..), webToEditCategory)

updateCategory :: (Monad m) => Proxy 'AdminRole -> Handle m -> Request -> m Response
updateCategory _ h req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit Category WEB"
  body <- webToEditCategory <$> getBody h req
  case body of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode Edit Category WEB"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error (T.pack e)
      pure (response400 h . T.pack $ e)
    Right (EditCategoryFromWeb {newlabel = (Just newlabel'), ..}) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "try edit category Just newlabel parent"
      tryEditCategory <-
        updateCategoryBase
          baseHandle
          (MkLabel label)
          ( CategoryInternal
              { labelCategory = MkLabel newlabel',
                parentCategory = fmap MkLabel newparent
              }
          )
      case tryEditCategory of
        Right _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit Category success WEB"
          pure $ response200 h
        Left _ -> pure $ response500 h
    Right (EditCategoryFromWeb {newlabel = Nothing, ..}) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "try edit category without new label"
      tryEditCategory <-
        updateCategoryBase
          baseHandle
          (MkLabel label)
          ( CategoryInternal
              { labelCategory = MkLabel label,
                parentCategory = fmap MkLabel newparent
              }
          )
      case tryEditCategory of
        Right _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Edit Category success WEB"
          pure $ response200 h
        Left _ -> pure $ response500 h
