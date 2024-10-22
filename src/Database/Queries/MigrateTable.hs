module Database.Queries.MigrateTable (isMigrateTable) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (listToMaybe)
import Database.Esqueleto.Experimental (desc, from, limit, orderBy, select, table, unValue, (^.))
import Database.Migrations.Type (EntityField (..), MigrateTable (..))
import Database.Persist.Postgresql (ConnectionString)
import Database.Persist.Sql (SqlPersistT)
import Database.Verb (runDataBaseWithOutLog)

type Version = Int

isMigrateTable :: ConnectionString -> IO (Either SomeException (Maybe Version))
isMigrateTable pginfo =
  try @SomeException (runDataBaseWithOutLog pginfo fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe Version)
    fetchAction = do
      version <- select $ do
        migrates <- from $ table @MigrateTable
        orderBy [desc (migrates ^. MigrateTableVersion)]
        limit 1
        pure (migrates ^. MigrateTableVersion)
      pure . listToMaybe . fmap unValue $ version
