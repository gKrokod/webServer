{-# LANGUAGE RecordWildCards #-}

module Database.Queries.User (pullAllUsers, findUserByLogin, putUser) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO)
import Data.Time (UTCTime (..))
import Database.Esqueleto.Experimental (from, getBy, insert, limit, offset, select, table)
import Database.Persist.Postgresql (ConnectionString, Entity (..))
import Database.Persist.Sql (SqlPersistT)
import Database.Verb (runDataBaseWithOutLog)
import Handlers.Database.Base (Limit (..), Offset (..), Success (..))
import Handlers.Web.Base (UserInternal (..))
import Schema (Password (..), Unique (..), User (..))
import Types (Login (..), Name (..), PasswordUser (..))

putUser :: ConnectionString -> UserInternal -> UTCTime -> IO (Either SomeException Success)
putUser pginfo (UserInternal {..}) time = do
  try @SomeException
    ( runDataBaseWithOutLog pginfo $ do
        pId <- insert $ Password {passwordQuasiPassword = getPasswordUser passwordUser}
        _ <-
          insert $
            User
              { userName = getName nameUser,
                userLogin = getLogin loginUser,
                userPasswordId = pId,
                userCreated = time,
                userIsAdmin = isAdminUser,
                userIsPublisher = isPublisherUser,
                userLastName = Just (getName nameUser)
              }
        pure Put
    )

findUserByLogin :: ConnectionString -> Login -> IO (Either SomeException (Maybe User))
findUserByLogin connString login = try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe User)
    fetchAction = (fmap . fmap) entityVal (getBy . UniqueUserLogin . getLogin $ login)

type LimitData = Int

pullAllUsers :: ConnectionString -> LimitData -> Offset -> Limit -> IO (Either SomeException [User])
pullAllUsers connString configLimit userOffset userLimit = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m [User]
    fetchAction =
      (fmap . fmap)
        entityVal
        ( select $ do
            users <- from $ table @User
            offset (fromIntegral . getOffset $ userOffset)
            limit (fromIntegral . min configLimit . getLimit $ userLimit)
            pure users
        )
