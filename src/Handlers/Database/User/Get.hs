module Handlers.Database.User.Get (getAllUsers) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Either (isLeft)
import qualified Data.Text as T
import Handlers.Database.Base (Handle (..), Limit (..), Offset (..))
import Handlers.Logger (Log (..), logMessage)
import Schema (User (..))

getAllUsers :: (Monad m) => Handle m -> m (Either T.Text [User])
getAllUsers h = do
  let logHandle = logger h
  users <- pullAllUsers h (MkOffset . userOffset $ h) (MkLimit . userLimit $ h)
  when (isLeft users) (logMessage logHandle Handlers.Logger.Error "function pullAllUsers fail")
  pure $ either (Left . T.pack . displayException) Right users
