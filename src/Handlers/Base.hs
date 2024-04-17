module Handlers.Base where
import Users
import Images
import Category
import News
import qualified Handlers.Logger

data Handle m = Handle {
  updateUser :: User -> m (),
  updateNews :: News -> m (),
  takeUsers :: m Users,
  takeNews :: m [News],
  takeCategories :: m CategoryDictionary,
  updateCategories :: CategoryDictionary -> m (),
  findImage :: Int -> m (Maybe Image),
  logger :: Handlers.Logger.Handle m
                       }


