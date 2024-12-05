module Handlers.Database.News.Get (getAllNews) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Either (isLeft)
import qualified Data.Text as T
import Handlers.Database.Base (Handle (..), Limit (..), Offset (..))
import Handlers.Logger (Log (..), logMessage)
import Handlers.Web.Base (NewsOut (..))

getAllNews :: (Monad m) => Handle m -> m (Either T.Text [NewsOut])
getAllNews h = do
  let logHandle = logger h
  news <- pullAllNews h (MkOffset . userOffset $ h) (MkLimit . userLimit $ h) (sortColumnNews h) (sortOrderNews h) (findSubString h) (filtersNews h)
  when (isLeft news) (logMessage logHandle Handlers.Logger.Error "function pullAllNews fail")
  pure $ either (Left . T.pack . displayException) Right news
