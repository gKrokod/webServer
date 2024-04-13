module Handlers.Base where
import Users
import Images
import qualified Handlers.Logger

data Handle m = Handle {
  updateUser :: User -> m (),
  takeUsers :: m Users,
  findImage :: Int -> m (Maybe Image),
  logger :: Handlers.Logger.Handle m
                       }


