-- {-# LANGUAGE DataKinds #-}
module Handlers.Web.Category.CategoryApi (endPointCategories) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Handlers.Database.Base
import qualified Handlers.Logger
import Handlers.Web.Base (Client (..), Handle (..))
import Handlers.Web.Category.Create (createCategory)
import Handlers.Web.Category.Get (existingCategories)
import Handlers.Web.Category.Update (updateCategory)
import Network.Wai (Request, Response, queryString, rawPathInfo)
import Web.WebType (queryToPanigate)

endPointCategories :: (Monad m) => Handle m -> Request -> m Response
endPointCategories h req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "end Point Categories"
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (E.decodeUtf8 $ rawPathInfo req)
  case rawPathInfo req of
    "/categories/create" -> do
      case client h of
        Client {clientAdminToken = (Just adminRole)} -> createCategory adminRole h req -- create a category for only admin
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure (response404 h)
    "/categories/edit" -> do
      case client h of
        Client {clientAdminToken = (Just adminRole)} -> updateCategory adminRole h req -- edit a category for only admin
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure (response404 h)
    "/categories" -> do
      let queryLimit = queryString req
          (userOffset, userLimit) = queryToPanigate queryLimit
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Query String:"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show queryLimit)
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show (userOffset, userLimit))

      let newBaseHandle = baseHandle {Handlers.Database.Base.userOffset = userOffset, Handlers.Database.Base.userLimit = userLimit}
      existingCategories (h {base = newBaseHandle}) req
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point not found"
      pure $ response404 h
