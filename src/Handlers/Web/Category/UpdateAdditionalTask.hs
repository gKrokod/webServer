{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.Web.Category.UpdateAdditionalTask (updateCategoryAdditionalTask) where

import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Handlers.Database.Api (updateCategoryBaseAdditionalTask)
import qualified Handlers.Logger
import Handlers.Web.Base (ClientRole (..), Handle (..))
import Handlers.Web.Category.Types (CategoryInternalId (..))
import Network.Wai (Request, Response)
import Types (Label (..), NumberCategory (..))
import Web.DTO.Category (EditCategoryIdFromWeb (..), webToEditIdCategory)

updateCategoryAdditionalTask :: (Monad m) => Proxy 'AdminRole -> Handle m -> Request -> m Response
updateCategoryAdditionalTask _ h req = do
  let logHandle = logger h
      baseHandle = base h
  body <- webToEditIdCategory <$> getBody h req
  case body of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error (T.pack e)
      pure (response400 h . T.pack $ e)
    Right (EditCategoryIdFromWeb {..}) -> do
      tryEditCategory <-
        updateCategoryBaseAdditionalTask
          baseHandle
          ( CategoryInternalId
              { idCategory = MkNumberCategory uid,
                labelCategory = fmap MkLabel newlabel,
                parentCategory = fmap MkLabel newparent
              }
          )
      case tryEditCategory of
        Right _ ->
          pure $ response200 h
        Left _ -> pure $ response500 h
