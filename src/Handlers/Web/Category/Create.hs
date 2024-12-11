{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.Web.Category.Create (createCategory) where

import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Handlers.Database.Api (createCategoryBase)
import qualified Handlers.Logger
import Handlers.Web.Base (ClientRole (..), Handle (..))
import Handlers.Web.Category.Types (CategoryInternal (..))
import Network.Wai (Request, Response)
import Types (Label (..))
import Web.DTO.Category (CategoryFromWeb (..), webToCategory)

createCategory :: (Monad m) => Proxy 'AdminRole -> Handle m -> Request -> m Response
createCategory _ h req = do
  let logHandle = logger h
      baseHandle = base h
  body <- webToCategory <$> getBody h req
  case body of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error (T.pack e)
      pure (response400 h . T.pack $ e)
    Right (CategoryFromWeb {..}) -> do
      tryCreateCategory <-
        createCategoryBase
          baseHandle
          ( CategoryInternal
              { labelCategory = MkLabel label,
                parentCategory = fmap MkLabel parent
              }
          )
      case tryCreateCategory of
        Right _ ->
          pure $ response200 h
        Left e -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning e
          pure $ response500 h
