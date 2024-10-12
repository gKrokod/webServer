module Database.Queries.Image (pullImage) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO)
import Database.Esqueleto.Experimental (get)
import Database.Persist.Postgresql (ConnectionString, toSqlKey)
import Database.Persist.Sql (SqlPersistT)
import Database.Verb (runDataBaseWithOutLog)
import Scheme (Image (..))
import Types (NumberImage (..))

pullImage :: ConnectionString -> NumberImage -> IO (Either SomeException (Maybe Image))
pullImage connString (MkNumberImage uid) = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe Image)
    fetchAction = get (toSqlKey uid)
