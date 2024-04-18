module Handlers.Base where
import Users
import Images
import Category
import News
import qualified Handlers.Logger
import qualified Data.Text as T

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
type Title = T.Text

findNews :: (Monad m) => Handle m -> Title -> m (Maybe News)
findNews h title = do
  ns <- takeNews h
  pure $ helper title ns 

helper :: Title -> [News] -> Maybe News
helper t []= Nothing
helper t (n : ns) | title n == t = Just n
                  | otherwise = helper t ns

  
