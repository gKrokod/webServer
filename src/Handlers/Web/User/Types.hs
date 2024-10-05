module Handlers.Web.User.Types (UserInternal (..)) where

import Types (Login (..), Name (..), PasswordUser (..))

data UserInternal = UserInternal
  { nameUser :: Name,
    loginUser :: Login,
    passwordUser :: PasswordUser,
    isAdminUser :: Bool,
    isPublisherUser :: Bool
  }
