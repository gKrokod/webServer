module Handlers.Base where
import Users
import Images
import Category
import qualified Handlers.Logger

data Handle m = Handle {
  updateUser :: User -> m (),
  takeUsers :: m Users,
  takeCategories :: m CategoryDictionary,
  updateCategories :: CategoryDictionary -> m (),
  findImage :: Int -> m (Maybe Image),
  logger :: Handlers.Logger.Handle m
                       }


