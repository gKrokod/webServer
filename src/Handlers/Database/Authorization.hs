{-# LANGUAGE RecordWildCards #-}

module Handlers.Database.Authorization (getPrivilege, getCopyRight, getResultValid) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Bool (bool)
import Data.Either (isLeft)
import qualified Data.Text as T
-- import Handlers.Database.Base (Handle (..))
import Handlers.Database.News (Handle (..))
import qualified Handlers.Database.News 
import Handlers.Logger (Log (..), logMessage)
import Schema (IsValidPassword (..), User (..))
import Types (Login (..), PasswordUser (..), Title (..))

getCopyRight :: (Monad m) => Handle m -> Login -> Title -> m (Either T.Text IsValidPassword)
getCopyRight h login title = do
  let logHandle = logger h
  tryValid <- validCopyRight h login title
  when (isLeft tryValid) (logMessage logHandle Handlers.Logger.Error "function validCopyRight fail")
  pure $ either (Left . T.pack . displayException) (Right . bool NotValid Valid) tryValid

getResultValid :: (Monad m) => Handle m -> Login -> PasswordUser -> m (Either T.Text IsValidPassword)
getResultValid h login password = do
  let logHandle = logger h
  tryValid <- validPassword h login password
  when (isLeft tryValid) (logMessage logHandle Handlers.Logger.Error "function validPassword fail")
  pure $ either (Left . T.pack . displayException) (Right . bool NotValid Valid) tryValid

type IsAdmin = Bool

type IsPublisher = Bool

getPrivilege :: (Monad m) => Handle m -> Login -> m (Either T.Text (IsAdmin, IsPublisher))
getPrivilege h login = do
  let logHandle = logger h
  tryFindUser <- findUserByLogin h login -- findUserBYLogin берется из общего handle base.
  when (isLeft tryFindUser) (logMessage logHandle Error "function findUserByLogin fail")
  case tryFindUser of
    Left e -> pure . Left . T.pack . displayException $ e
    Right (Just (User {..})) -> do
      pure $ Right (userIsAdmin, userIsPublisher)
    _ ->
      pure $ Right (False, False)
  -- let logHandle = Handlers.Web.Base.logger h
  --     userHandle = Handlers.Web.Base.user h
  --     baseUserHandle = Handlers.Web.User.base userHandle
  --     clientHandle = Handlers.Web.Base.client h
