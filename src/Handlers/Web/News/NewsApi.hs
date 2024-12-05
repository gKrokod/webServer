module Handlers.Web.News.NewsApi (endPointNews) where

import qualified Handlers.Database.Base
import qualified Handlers.Logger
import Handlers.Web.Base (Client (..), Handle (..))
import Handlers.Web.News.Create (createNews)
import Handlers.Web.News.Get (existingNews)
import Handlers.Web.News.Update (updateNews)
import Network.HTTP.Types (Query)
import Network.Wai (Request, Response, queryString, rawPathInfo)
import Schema (FilterItem (FilterPublishOrAuthor))
import Types (Login (..))
import Web.WebType (queryToFilters, queryToFind, queryToPaginate, queryToSort)

endPointNews :: (Monad m) => Handle m -> Request -> m Response
endPointNews h req = do
  let logHandle = logger h
  case rawPathInfo req of
    "/news/create" -> do
      case client h of
        Client {clientPublisherToken = (Just publisherRole)} -> createNews publisherRole h req
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure $ response403 h
    "/news/edit" ->
      case client h of
        Client {author = (Just author_)} -> do
          updateNews author_ h req
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure $ response403 h
    "/news" -> do
      let queryLimit = queryString req
      existingNews (foldSets queryLimit h [setFilters, setFind, setSort, setPaginate]) req
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point not found"
      pure $ response404 h
  where
    foldSets :: (Monad m) => Query -> Handle m -> [Handle m -> Query -> Handle m] -> Handle m
    foldSets query = foldr (\set h' -> set h' query)

    setPaginate :: (Monad m) => Handle m -> Query -> Handle m
    setPaginate h' q =
      let baseHandle = base h'
          (userOffset, userLimit) = queryToPaginate q
          newBaseHandle = baseHandle {Handlers.Database.Base.userOffset = userOffset, Handlers.Database.Base.userLimit = userLimit}
       in h' {base = newBaseHandle}

    setSort :: (Monad m) => Handle m -> Query -> Handle m
    setSort h' q =
      let baseHandle = base h'
          (userSortColumn, userSortOrder) = queryToSort q
          newBaseHandle = baseHandle {Handlers.Database.Base.sortColumnNews = userSortColumn, Handlers.Database.Base.sortOrderNews = userSortOrder}
       in h' {base = newBaseHandle}

    setFind :: (Monad m) => Handle m -> Query -> Handle m
    setFind h' q =
      let baseHandle = base h'
          mbFind = queryToFind q
          newBaseHandle = baseHandle {Handlers.Database.Base.findSubString = mbFind}
       in h' {base = newBaseHandle}

    setFilters :: (Monad m) => Handle m -> Query -> Handle m
    setFilters h' q =
      let baseHandle = base h'
          filters = queryToFilters q
          filterVisible = FilterPublishOrAuthor (fmap getLogin $ author $ client h)
          newBaseHandle = baseHandle {Handlers.Database.Base.filtersNews = filterVisible : filters}
       in h' {base = newBaseHandle}
