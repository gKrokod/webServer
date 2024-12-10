module Handlers.Web.Category.CategoryApi (endPointCategories) where

import qualified Handlers.Database.Base
import qualified Handlers.Logger
import Handlers.Web.Base (Client (..), Handle (..))
import Handlers.Web.Category.Create (createCategory)
import Handlers.Web.Category.Get (existingCategories)
import Handlers.Web.Category.Update (updateCategory)
import Network.Wai (Request, Response, queryString, rawPathInfo)
import Web.QueryTransfer (queryToPaginate)

endPointCategories :: (Monad m) => Handle m -> Request -> m Response
endPointCategories h req = do
  let logHandle = logger h
      baseHandle = base h
  case rawPathInfo req of
    "/categories/create" -> do
      case client h of
        Client {clientAdminToken = (Just adminRole)} -> createCategory adminRole h req
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure $ response404 h
    "/categories/edit" -> do
      case client h of
        Client {clientAdminToken = (Just adminRole)} -> updateCategory adminRole h req
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure $ response404 h
    "/categories" -> do
      let queryLimit = queryString req
          (userOffset, userLimit) = queryToPaginate queryLimit

      let newBaseHandle = baseHandle {Handlers.Database.Base.userOffset = userOffset, Handlers.Database.Base.userLimit = userLimit}
      existingCategories (h {base = newBaseHandle}) req
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point not found"
      pure $ response404 h
