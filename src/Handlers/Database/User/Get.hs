module Handlers.Database.User.Get (getAllUsers) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Either (isLeft)
import qualified Data.Text as T
import Handlers.Database.Base (Limit (..), Offset (..))
import Handlers.Database.User (Handle (..))
import Handlers.Logger (Log (..), logMessage)
import Schema (User (..))

getAllUsers :: (Monad m) => Handle m -> Offset -> Limit -> m (Either T.Text [User])
getAllUsers h offset limit = do
  let logHandle = logger h
  users <- pullAllUsers h offset limit
  when (isLeft users) (logMessage logHandle Handlers.Logger.Error "function pullAllUsers fail")
  pure $ either (Left . T.pack . displayException) Right users
