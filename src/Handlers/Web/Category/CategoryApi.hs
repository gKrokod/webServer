module Handlers.Web.Category.CategoryApi (endPointCategories) where

import Handlers.Database.Auth (Client (..))
import qualified Handlers.Database.Category
import qualified Handlers.Logger
import Handlers.Web.Base (Handle (..))
import qualified Handlers.Web.Category
import Handlers.Web.Category.Create (createCategory)
import Handlers.Web.Category.Get (existingCategories)
import Handlers.Web.Category.Update (updateCategory)
import Network.Wai (Request, Response, queryString, rawPathInfo)
import Web.Query (queryToPaginate)
import qualified Web.Utils as WU

endPointCategories :: (Monad m) => Handle m -> Request -> m Response
endPointCategories h req = do
  let logHandle = Handlers.Web.Base.logger h
      categoryHandle = Handlers.Web.Base.category h
      baseCategoryHandle = Handlers.Web.Category.base categoryHandle
      userRole = Handlers.Web.Base.client h
  case rawPathInfo req of
    "/categories/create" -> do
      case userRole of
        Client {clientAdminToken = (Just adminRole)} -> createCategory adminRole categoryHandle req
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure $ WU.response404
    "/categories/edit" -> do
      case userRole of
        Client {clientAdminToken = (Just adminRole)} -> updateCategory adminRole categoryHandle req
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure $ WU.response404
    "/categories" -> do
      let queryLimit = queryString req
          (userOffset, userLimit) = queryToPaginate queryLimit

      let newBaseCategoryHandle = baseCategoryHandle {Handlers.Database.Category.userOffset = userOffset, Handlers.Database.Category.userLimit = userLimit}
      existingCategories (categoryHandle {Handlers.Web.Category.base = newBaseCategoryHandle}) req
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point not found"
      pure $ WU.response404 
