module Handlers.Web.User.Get.Api (existingUsers) where

import Handlers.Base.Api (getAllUsers)
import qualified Handlers.Logger
import Handlers.Web.Web (Handle (..))
import Network.Wai (Request, Response)
import Web.WebType (userToWeb)

existingUsers :: (Monad m) => Handle m -> Request -> m Response -- for ALl
existingUsers h _req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Get All users"
  getUsers <- getAllUsers baseHandle
  case getUsers of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error e
      pure $ response404 h -- "Not ok.
    Right users -> pure . mkGoodResponse h . userToWeb $ users
