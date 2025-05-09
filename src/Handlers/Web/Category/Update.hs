{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.Web.Category.Update (updateCategory) where

import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Handlers.Database.Api (updateCategoryBase)
import Handlers.Database.Auth (ClientRole (..))
import qualified Handlers.Logger
import Handlers.Web.Category (Handle (..))
import Handlers.Web.Category.Types (CategoryInternal (..))
import Network.Wai (Request, Response)
import Types (Label (..))
import Web.DTO.Category (EditCategoryFromWeb (..), webToEditCategory)
import qualified Web.Utils as WU

updateCategory :: (Monad m) => Proxy 'AdminRole -> Handle m -> Request -> m Response
updateCategory _ h req = do
  let logHandle = logger h
      baseHandle = base h
  body <- webToEditCategory <$> getBody h req
  case body of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error (T.pack e)
      pure (WU.response400 . T.pack $ e)
    Right (EditCategoryFromWeb {newlabel = (Just newlabel'), ..}) -> do
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
        Right _ ->
          pure WU.response200
        Left _ -> pure WU.response500
    Right (EditCategoryFromWeb {newlabel = Nothing, ..}) -> do
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
        Right _ ->
          pure WU.response200
        Left _ -> pure WU.response500
