{-# LANGUAGE RecordWildCards #-}

module Handlers.Database.User.Create (createUserBase) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Either (isLeft)
import qualified Data.Text as T
import Handlers.Database.Base (Handle (..), Success (..))
import Handlers.Logger (Log (..), logMessage)
import Handlers.Web.Base (UserInternal (..))
import Types (Login (..), PasswordUser (..))

createUserBase :: (Monad m) => Handle m -> UserInternal -> m (Either T.Text Success)
createUserBase h user@(UserInternal {..}) = do
  let logHandle = logger h
  tryFind <- findUserByLogin h loginUser
  case tryFind of
    Left e -> do
      logMessage logHandle Error "function findUserByLogin fail"
      pure . Left . T.pack . displayException $ e
    Right (Just _) -> do
      logMessage logHandle Warning ("Login arleady taken: " <> getLogin loginUser)
      pure $ Left "Login arleady taken"
    Right Nothing -> do
      time <- getTime h
      let pwd' = makeHashPassword h passwordUser time
      tryCreate <- putUser h (user {passwordUser = MkPasswordUser pwd'}) time
      when (isLeft tryCreate) (logMessage logHandle Handlers.Logger.Error "Can't putUser")
      pure $ either (Left . T.pack . displayException) Right tryCreate
