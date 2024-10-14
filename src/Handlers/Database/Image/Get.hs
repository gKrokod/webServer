module Handlers.Database.Image.Get (getImage) where

import qualified Data.Text as T
import Handlers.Database.Base (Handle (..))
import Handlers.Logger (Log (..), logMessage)
import Schema (Image (..))
import Types (NumberImage (..))

getImage :: (Monad m) => Handle m -> NumberImage -> m (Either T.Text Image)
getImage h uid = do
  let logHandle = logger h
  logMessage logHandle Debug ("Try to get image from database " <> (T.pack . show $ uid))
  images <- pullImage h uid
  case images of
    Left e -> do
      logMessage logHandle Handlers.Logger.Error "function pullImage fail"
      pure . Left . T.pack . show $ e
    Right Nothing -> do
      logMessage logHandle Debug "Image was not found in database"
      pure $ Left "Image was not found in database"
    Right (Just image) -> do
      logMessage logHandle Debug "Image was found in database"
      pure $ Right image
