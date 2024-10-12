{-# LANGUAGE DataKinds #-}

module Handlers.Web.Category.Create (createCategory) where

import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Handlers.Database.Api (createCategoryBase)
import qualified Handlers.Logger
import Handlers.Web.Base (ClientRole (..), Handle (..))
import Handlers.Web.Category.Types (CategoryInternal (..))
import Network.Wai (Request, Response)
import Types (Label (..))
import Web.WebType (CategoryFromWeb (..), webToCategory)

createCategory :: (Monad m) => Proxy 'AdminRole -> Handle m -> Request -> m Response
createCategory _ h req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create Category WEB"
  body <- webToCategory <$> getBody h req -- :: (Either String CategoryFromWeb)
  case body of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode Category WEB"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)
      pure (response404 h) -- "Not ok.
    Right (CategoryFromWeb label_ parent_) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "try create category"
      tryCreateCategory <-
        createCategoryBase
          baseHandle
          ( CategoryInternal
              { labelCategory = MkLabel label_,
                parentCategory = fmap MkLabel parent_
              }
          )
      case tryCreateCategory of
        Right _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create Category success WEB"
          pure $ response200 h
        Left e -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e
          pure $ response404 h -- "Not ok.
