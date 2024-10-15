{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.Web.User.Create (createUser) where

import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Handlers.Database.Api (createUserBase)
import qualified Handlers.Logger
import Handlers.Web.Base (ClientRole (..), Handle (..))
import Handlers.Web.User.Types (UserInternal (..))
import Network.Wai (Request, Response)
import Types (Login (..), Name (..), PasswordUser (..))
import Web.WebType (UserFromWeb (..), webToUser)

createUser :: (Monad m) => Proxy 'AdminRole -> Handle m -> Request -> m Response -- for Admin
createUser _ h req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "create User WEB"
  body <- webToUser <$> getBody h req 
  case body of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode User WEB"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)
      pure (response404 h)
    Right (UserFromWeb {..}) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Try to create user WEB"
      tryCreateUser <-
        createUserBase
          baseHandle
          ( UserInternal
              { nameUser = MkName name,
                loginUser = MkLogin login,
                passwordUser = MkPasswordUser password,
                isAdminUser = isAdmin,
                isPublisherUser = isPublisher
              }
          )
      case tryCreateUser of
        Right _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create User success WEB"
          pure (response200 h)
        Left e -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e
          pure $ response404 h 
