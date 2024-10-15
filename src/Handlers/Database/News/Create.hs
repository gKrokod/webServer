{-# LANGUAGE RecordWildCards #-}

module Handlers.Database.News.Create (createNewsBase) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Either (isLeft)
import qualified Data.Text as T
import Handlers.Database.Base (Handle (..), Success (..))
import Handlers.Logger (Log (..), logMessage)
import Handlers.Web.Base (NewsInternal (..))
import Types (Label (..), Login (..), Title (..))

createNewsBase :: (Monad m) => Handle m -> NewsInternal -> m (Either T.Text Success)
createNewsBase h news@(NewsInternal {..}) = do
  let logHandle = logger h
  logMessage logHandle Debug ("Check news by title for create: " <> getTitle titleNews)
  existTitle <- findNewsByTitle h titleNews
  when (isLeft existTitle) (logMessage logHandle Handlers.Logger.Error "function findNewsByTitle fail")

  logMessage logHandle Debug ("Check user by login for create: " <> getLogin authorNews)
  existUser <- findUserByLogin h authorNews
  when (isLeft existUser) (logMessage logHandle Handlers.Logger.Error "function findUserByLogin fail")

  logMessage logHandle Debug ("Check category by label for create: " <> getLabel labelNews)
  existCategory <- findCategoryByLabel h labelNews
  when (isLeft existCategory) (logMessage logHandle Handlers.Logger.Error "function findCategoryByLabel fail")

  case (existTitle, existUser, existCategory) of
    (Right Nothing, Right (Just _user), Right (Just _category)) -> do
      logMessage logHandle Debug ("Create news with title, login and label: " <> getTitle titleNews <> " " <> getLogin authorNews <> " " <> getLabel labelNews)
      time <- getTime h
      tryPut <- putNews h news time
      when (isLeft tryPut) (logMessage logHandle Handlers.Logger.Error "function putNews fail")
      pure $ either (Left . T.pack . displayException) Right tryPut
    _ -> do
      logMessage logHandle Warning ("Fail to create news with title, login and label: " <> getTitle titleNews <> " " <> getLogin authorNews <> " " <> getLabel labelNews)
      pure $ Left "fail to create news"
