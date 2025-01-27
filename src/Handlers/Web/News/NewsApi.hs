module Handlers.Web.News.NewsApi (endPointNews) where

import qualified Handlers.Database.News
import qualified Handlers.Web.News
import qualified Handlers.Logger
import qualified Handlers.Database.Auth as A (Client (..), Handle (..))
import Handlers.Web.Base (Handle (..))
import Handlers.Web.News.Create (createNews)
import Handlers.Web.News.Get (existingNews)
import Handlers.Web.News.Update (updateNews)
import Network.HTTP.Types (Query)
import Network.Wai (Request, Response, queryString, rawPathInfo)
import Schema (FilterItem (FilterPublishOrAuthor))
import Types (Login (..))
import Web.Query (queryToFilters, queryToFind, queryToPaginate, queryToSort)

endPointNews :: (Monad m) => Handle m -> Request -> m Response
endPointNews h req = do
  let logHandle = Handlers.Web.Base.logger h
      newsHandle = Handlers.Web.Base.news h
      -- baseNewsHandle = Handlers.Web.News.base newsHandle
      userRole = Handlers.Web.Base.client h
  case rawPathInfo req of
    "/news/create" -> do
      case userRole of
        A.Client {A.clientPublisherToken = (Just publisherRole)} -> createNews publisherRole newsHandle req
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure $ Handlers.Web.News.response403 newsHandle
    "/news/edit" ->
      case userRole of
        A.Client {A.author = (Just author_)} -> do
          updateNews author_ newsHandle req
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure $ Handlers.Web.News.response403 newsHandle
    "/news" -> do
      let queryLimit = queryString req
      existingNews (foldSets queryLimit newsHandle [setFilters, setFind, setSort, setPaginate]) req
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point not found"
      pure $ Handlers.Web.News.response404 newsHandle
  where
    foldSets :: (Monad m) => Query -> Handlers.Web.News.Handle m -> [Handlers.Web.News.Handle m -> Query -> Handlers.Web.News.Handle m] -> Handlers.Web.News.Handle m
    foldSets query = foldr (\set h' -> set h' query)

    setPaginate :: (Monad m) => Handlers.Web.News.Handle m -> Query -> Handlers.Web.News.Handle m
    setPaginate h' q =
      let baseHandle = Handlers.Web.News.base h'
          (userOffset, userLimit) = queryToPaginate q
          newBaseHandle = baseHandle {Handlers.Database.News.userOffset = userOffset, Handlers.Database.News.userLimit = userLimit}
       in h' {Handlers.Web.News.base = newBaseHandle}

    setSort :: (Monad m) => Handlers.Web.News.Handle m -> Query -> Handlers.Web.News.Handle m
    setSort h' q =
      let baseHandle = Handlers.Web.News.base h'
          (userSortColumn, userSortOrder) = queryToSort q
          newBaseHandle = baseHandle {Handlers.Database.News.sortColumnNews = userSortColumn, Handlers.Database.News.sortOrderNews = userSortOrder}
       in h' {Handlers.Web.News.base = newBaseHandle}

    setFind :: (Monad m) => Handlers.Web.News.Handle m -> Query -> Handlers.Web.News.Handle m
    setFind h' q =
      let baseHandle = Handlers.Web.News.base h'
          mbFind = queryToFind q
          newBaseHandle = baseHandle {Handlers.Database.News.findSubString = mbFind}
       in h' {Handlers.Web.News.base = newBaseHandle}

    setFilters :: (Monad m) => Handlers.Web.News.Handle m -> Query -> Handlers.Web.News.Handle m
    setFilters h' q =
      let baseHandle = Handlers.Web.News.base h'
          authHandle = Handlers.Web.News.auth h'
          filters = queryToFilters q
          filterVisible = FilterPublishOrAuthor (fmap getLogin $ A.author $ A.client authHandle)
          -- filterVisible = FilterPublishOrAuthor (fmap getLogin $ author $ undefined)
          newBaseHandle = baseHandle {Handlers.Database.News.filtersNews = filterVisible : filters}
       in h' {Handlers.Web.News.base = newBaseHandle}
