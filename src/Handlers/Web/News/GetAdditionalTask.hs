module Handlers.Web.News.GetAdditionalTask (existingNewsAdditionalTask) where

import Data.ByteString.Char8 as BC (readInt)
import Handlers.Database.Api (getOneNews)
import qualified Handlers.Logger
import Handlers.Web.Base (Handle (..))
import Network.Wai (Request, Response, queryString)
import Types (NumberNews (..))
import Web.DTO.News (newsWithIdToWeb)

existingNewsAdditionalTask :: (Monad m) => Handle m -> Request -> m Response
existingNewsAdditionalTask h req = do
  let logHandle = logger h
      baseHandle = base h
      queryNews = queryString req
  case queryNews of
    [("id", Just n)] | idNews > 0 -> do
      eNews <- getOneNews baseHandle (MkNumberNews idNews)
      case eNews of
        Left e -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e
          pure $ response500 h
        Right news' -> pure . mkGoodResponse h . newsWithIdToWeb $ [news']
      where
        idNews = maybe (-1) (fromIntegral . fst) (BC.readInt n)
    _ -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error "Bad request News"
      pure $ response400 h "Bad request \n"
