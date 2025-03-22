module Handlers.Web.Category.CategoryApi (endPointCategories) where

import Handlers.Database.Auth (Client (..))
import qualified Handlers.Logger
import Handlers.Web.Base (Handle (..))
import Handlers.Web.Category.Create (createCategory)
import Handlers.Web.Category.Get (existingCategories)
import Handlers.Web.Category.Update (updateCategory)
import Network.Wai (Request, Response, rawPathInfo)
import qualified Web.Utils as WU

endPointCategories :: (Monad m) => Handle m -> Request -> m Response
endPointCategories h req = do
  let logHandle = Handlers.Web.Base.logger h
      categoryHandle = Handlers.Web.Base.category h
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
    "/categories" -> existingCategories categoryHandle req
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point not found"
      pure $ WU.response404 
