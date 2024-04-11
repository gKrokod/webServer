module Handlers.Base where
import Users
import qualified Handlers.Logger

data Handle m = Handle {
  updateUser :: User -> m (),
  takeUsers :: m ([User]),
  logger :: Handlers.Logger.Handle m
                       }
