module Handlers.Web.News.Get (existingNews) where

import Handlers.Database.Api (getAllNews)
import qualified Handlers.Logger
import Handlers.Web.News (Handle (..))
import Network.Wai (Request, Response)
import Web.DTO.News (newsToWeb)
import qualified Web.Utils as WU
import Schema (FilterItem(..))
import Network.Wai (queryString) 
import Web.Query (queryToFilters, queryToFind, queryToPaginate, queryToSort)
import Handlers.Database.Base (Limit (..), Offset (..))

existingNews :: (Monad m) => FilterItem -> Handle m -> Request -> m Response
existingNews filterAuthor h req = do
  let logHandle = logger h
      baseHandle = base h
      query = queryString req
      (userOffset, userLimit) = queryToPaginate query
      (userSortColumn, userSortOrder) = queryToSort query
      findSubString = queryToFind query
      filters = filterAuthor : queryToFilters query
  news <- getAllNews baseHandle (MkOffset userOffset) (MkLimit userLimit) userSortColumn userSortOrder findSubString filters
  case news of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error e
      pure $ WU.response500
    Right news' -> pure . WU.mkGoodResponse . newsToWeb $ news'
