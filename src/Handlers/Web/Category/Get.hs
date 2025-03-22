module Handlers.Web.Category.Get (existingCategories) where

import Handlers.Database.Api (getAllCategories)
import qualified Handlers.Logger
import Handlers.Web.Category (Handle (..))
import Network.Wai (Request, Response)
import Web.DTO.Category (categoryToWeb)
import qualified Web.Utils as WU
import Handlers.Database.Base (Limit (..), Offset (..))
import Network.Wai (queryString) 
import Web.Query (queryToPaginate)

existingCategories :: (Monad m) => Handle m -> Request -> m Response
existingCategories h req = do
  let logHandle = logger h
      baseHandle = base h
      query = queryString req
      (userOffset, userLimit) = queryToPaginate query
  categories <- getAllCategories baseHandle (MkOffset userOffset) (MkLimit userLimit)
  case categories of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error e
      pure $ WU.response500
    Right c -> pure . WU.mkGoodResponse . categoryToWeb $ c
