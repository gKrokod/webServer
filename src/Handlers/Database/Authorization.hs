module Handlers.Database.Authorization (getPrivilege, getCopyRight, getResultValid) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Bool (bool)
import Data.Either (isLeft)
import qualified Data.Text as T
import Handlers.Database.Base (Handle (..))
import Handlers.Logger (Log (..), logMessage)
import Scheme (IsValidPassword (..), User (..))
import Types (Login (..), PasswordUser (..), Title (..))

getCopyRight :: (Monad m) => Handle m -> Login -> Title -> m (Either T.Text IsValidPassword)
getCopyRight h login title = do
  let logHandle = logger h
  logMessage logHandle Debug ("Check copyright for the login: " <> getLogin login <> " of the news with title: " <> getTitle title)
  tryValid <- validCopyRight h login title
  when (isLeft tryValid) (logMessage logHandle Handlers.Logger.Error "function validCopyRight fail")
  pure $ either (Left . T.pack . displayException) (Right . bool NotValid Valid) tryValid

getResultValid :: (Monad m) => Handle m -> Login -> PasswordUser -> m (Either T.Text IsValidPassword)
getResultValid h login password = do
  let logHandle = logger h
  logMessage logHandle Debug ("Check password for: " <> getLogin login <> " " <> getPasswordUser password)
  tryValid <- validPassword h login password
  when (isLeft tryValid) (logMessage logHandle Handlers.Logger.Error "function validPassword fail")
  pure $ either (Left . T.pack . displayException) (Right . bool NotValid Valid) tryValid

type IsAdmin = Bool

type IsPublisher = Bool

getPrivilege :: (Monad m) => Handle m -> Login -> m (Either T.Text (IsAdmin, IsPublisher))
getPrivilege h login = do
  let logHandle = logger h
  logMessage logHandle Debug ("Get privilege for login: " <> getLogin login)
  tryFindUser <- findUserByLogin h login
  when (isLeft tryFindUser) (logMessage logHandle Error "function findUserByLogin fail")
  case tryFindUser of
    Left e -> pure . Left . T.pack . displayException $ e
    Right (Just (User _ _ _ _ a p)) -> do
      logMessage logHandle Debug (T.pack $ "Privilege: Admin " <> show a <> " Publisher " <> show p)
      pure $ Right (a, p)
    _ -> do
      logMessage logHandle Debug "Privilege: Admin False False "
      pure $ Right (False, False)
