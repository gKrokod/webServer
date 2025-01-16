module Handlers.Web.User.Get (existingUsers) where

import Handlers.Database.Api (getAllUsers)
import qualified Handlers.Logger
import Handlers.Web.Base (HandleUser (..))
import Network.Wai (Request, Response)
import Web.DTO.User (userToWeb)

existingUsers :: (Monad m) => HandleUser m -> Request -> m Response
existingUsers h _req = do
  let logHandle = loggerUser h
      -- baseHandle = base h
  getUsers <- getAllUsers h 
  case getUsers of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error e
      pure $ response500U h
    Right users -> pure . mkGoodResponseU h . userToWeb $ users

-- existingUsers :: (Monad m) => Handle m -> Request -> m Response
-- existingUsers h _req = do
--   let logHandle = logger h
--       baseHandle = base h
--   getUsers <- getAllUsers baseHandle
--   case getUsers of
--     Left e -> do
--       Handlers.Logger.logMessage logHandle Handlers.Logger.Error e
--       pure $ response500 h
--     Right users -> pure . mkGoodResponse h . userToWeb $ users
