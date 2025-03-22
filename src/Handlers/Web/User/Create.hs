{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.Web.User.Create (createUser) where

import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Handlers.Database.Api (createUserBase)
import Handlers.Database.Auth (ClientRole (..))
import qualified Handlers.Logger
import Handlers.Web.User (Handle (..))
import Handlers.Web.User.Types (UserInternal (..))
import Network.Wai (Request, Response)
import Types (Login (..), Name (..), PasswordUser (..))
import Web.DTO.User (UserFromWeb (..), webToUser)
import qualified Web.Utils as WU

createUser :: (Monad m) => Proxy 'AdminRole -> Handle m -> Request -> m Response
createUser _ h req = do
  let logHandle = logger h
      baseHandle = base h
  body <- webToUser <$> getBody h req
  case body of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error (T.pack e)
      pure (WU.response400 . T.pack $ e)
    Right (UserFromWeb {..}) -> do
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
        Right _ ->
          pure $ WU.response200
        Left e -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Error e
          pure $ WU.response500
