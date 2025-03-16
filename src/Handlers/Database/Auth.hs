{-# LANGUAGE DataKinds #-}

module Handlers.Database.Auth (Handle (..), Client (..), ClientRole (..)) where

import Control.Exception (SomeException)
import Data.Proxy (Proxy (..))
import qualified Handlers.Logger
import Schema (User (..))
import Types (Login (..), PasswordUser (..), Title (..))

data ClientRole = AdminRole | PublisherRole
  deriving (Eq, Show)

data Client = Client
  { clientAdminToken :: Maybe (Proxy 'AdminRole),
    clientPublisherToken :: Maybe (Proxy 'PublisherRole),
    author :: Maybe Login
  }
  deriving (Eq, Show)

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    validPassword :: Login -> PasswordUser -> m (Either SomeException Bool),
    validCopyRight :: Login -> Title -> m (Either SomeException Bool),
    client :: Client,
    findUserByLogin :: Login -> m (Either SomeException (Maybe User))
  }
