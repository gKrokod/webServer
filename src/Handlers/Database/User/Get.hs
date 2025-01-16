module Handlers.Database.User.Get (getAllUsers) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Either (isLeft)
import qualified Data.Text as T
import Handlers.Database.Base ( Limit (..), Offset (..))
import Handlers.Web.Base (HandleUser (..))
import Handlers.Logger (Log (..), logMessage)
import Schema (User (..))

getAllUsers :: (Monad m) => HandleUser m -> m (Either T.Text [User])
getAllUsers h = do
  let logHandle = loggerUser h
  users <- pullAllUsers h (MkOffset . userOffsetU $ h) (MkLimit . userLimitU $ h)
  when (isLeft users) (logMessage logHandle Handlers.Logger.Error "function pullAllUsers fail")
  pure $ either (Left . T.pack . displayException) Right users
