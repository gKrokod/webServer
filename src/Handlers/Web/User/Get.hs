module Handlers.Web.User.Get (existingUsers) where

import Handlers.Database.Api (getAllUsers)
import Handlers.Database.Base (Limit (..), Offset (..))
import qualified Handlers.Logger
import Handlers.Web.User (Handle (..))
import Network.Wai (Request, Response, queryString)
import Web.DTO.User (userToWeb)
import Web.Query (queryToPaginate)
import qualified Web.Utils as WU

existingUsers :: (Monad m) => Handle m -> Request -> m Response
existingUsers h req = do
  let logHandle = logger h
      baseHandle = base h
      query = queryString req
      (userOffset, userLimit) = queryToPaginate query
  getUsers <- getAllUsers baseHandle (MkOffset userOffset) (MkLimit userLimit)
  case getUsers of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error e
      pure WU.response500
    Right users -> pure . WU.mkGoodResponse . userToWeb $ users
