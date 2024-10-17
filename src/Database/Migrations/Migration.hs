{-# LANGUAGE RecordWildCards #-}

module Database.Migrations.Migration (migrationEngine) where

import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (mapMaybe)
import Data.Time (getCurrentTime)
import Database.Esqueleto.Experimental (insert_)
import Database.Migrations.MigrationPlan (migrationPlan)
import Database.Migrations.Type (MigrateTable (..), MyMigration (..))
import Database.Persist.Postgresql (ConnectionString)
import Database.Persist.Sql (runMigration)
import Database.Queries.MigrateTable (isMigrateTable)
import Database.Verb (runDataBaseWithOutLog)

type Version = Int

migrationEngine :: ConnectionString -> IO ()
migrationEngine pginfo = do
  is <- isMigrateTable pginfo
  let migrationPlan' = mapMaybe (predicate is) migrationPlan
  runDataBaseWithOutLog pginfo $ do
    mapM_ applyMigrate migrationPlan'
  where
    applyMigrate m@(MkMigration {..}) = do
      liftIO $ putStrLn $ "Apply migration " <> show m
      runMigration content
      time <- liftIO getCurrentTime
      insert_ (MigrateTable {migrateTableVersion = version, migrateTableDescription = description, migrateTableTime = time})

    predicate :: Either SomeException (Maybe Version) -> MyMigration -> Maybe MyMigration
    predicate (Right (Just num)) (MkMigration {..}) | num >= version = Nothing
    predicate _ m = Just m
