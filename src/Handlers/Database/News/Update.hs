{-# LANGUAGE RecordWildCards #-}

module Handlers.Database.News.Update (updateNewsBase) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Either (isLeft)
import qualified Data.Text as T
import Handlers.Database.Base (Handle (..), Success (..))
import Handlers.Logger (Log (..), logMessage)
import Handlers.Web.Base (NewsEditInternal (..))
import Types (Title (..))

updateNewsBase :: (Monad m) => Handle m -> Title -> NewsEditInternal -> m (Either T.Text Success)
updateNewsBase h title newsEdit@(NewsEditInternal {..}) = do
  let logHandle = logger h
  logMessage logHandle Debug ("Checks attributes for update news with title " <> getTitle title)
  existTitle <-
    either
      (Left . T.pack . displayException)
      (maybe (Left "news don't exist") (\_ -> Right Change))
      <$> findNewsByTitle h title
  existNewTitle <- checkNews titleEditNews
  existUser <- checkUser authorEditNews
  existCategory <- checkCategory labelEditNews

  case sequence_ [existTitle, existNewTitle, existUser, existCategory] of
    Left e -> do
      logMessage
        logHandle
        Warning
        ( "Fail to update news with attributes: "
            <> (T.pack . show) title
            <> " "
            <> (T.pack . show) titleEditNews
            <> " "
            <> (T.pack . show) authorEditNews
            <> " "
            <> (T.pack . show) labelEditNews
            <> " "
        )
      logMessage logHandle Warning e
      pure $ Left "fail to update news"
    Right _ -> do
      logMessage logHandle Debug "Ok. Updates news..."
      t <- getTime h
      tryEdit <- editNews h title t newsEdit
      when (isLeft tryEdit) (logMessage logHandle Handlers.Logger.Error "Can't editNews")
      pure $ either (Left . T.pack . displayException) Right tryEdit
  where
    checkUser Nothing = pure $ Right Change
    checkUser (Just login) = do
      tryFindUser <- findUserByLogin h login
      when (isLeft tryFindUser) (logMessage (logger h) Error "function findUserByLogin fail")
      pure
        ( either
            (Left . T.pack . displayException)
            (maybe (Left "Fail update news. User don't exist!") (\_ -> Right Change))
            tryFindUser
        )
    checkCategory Nothing = pure (Right Change)
    checkCategory (Just label) = do
      tryFindCategory <- findCategoryByLabel h label
      when (isLeft tryFindCategory) (logMessage (logger h) Error "function findCategoryByLabel fail")
      pure
        ( either
            (Left . T.pack . displayException)
            (maybe (Left "Fail update news. Category don't exist!") (\_ -> Right Change))
            tryFindCategory
        )

    checkNews Nothing = pure (Right Change)
    checkNews (Just titleNews) = do
      tryFindNews <- findNewsByTitle h titleNews
      when (isLeft tryFindNews) (logMessage (logger h) Error "function findNewsByTitle fail")
      pure
        ( either
            (Left . T.pack . displayException)
            (maybe (Right Change) (\_ -> Left $ "Fail update news. News with your title is existed! : " <> getTitle titleNews))
            tryFindNews
        )
