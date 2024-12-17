module Handlers.Database.News.GetAdditionalTask (getOneNews) where

import qualified Data.Text as T
import Handlers.Database.Base (Handle (..))
import Handlers.Logger (Log (..), logMessage)
import Handlers.Web.Base (NewsOut (..))
import Types (NumberNews (..))

getOneNews :: (Monad m) => Handle m -> NumberNews -> m (Either T.Text NewsOut)
getOneNews h uid = do
  let logHandle = logger h
  news <- pullOneNews h uid
  case news of
    Left e -> do
      logMessage logHandle Handlers.Logger.Error "function pullOneNews fail"
      pure . Left . T.pack . show $ e
    Right news' -> do
      pure $ Right news'
