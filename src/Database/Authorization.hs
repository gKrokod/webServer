module Database.Authorization (validCopyRight, validPassword) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import qualified Database.Crypto
import Database.Esqueleto.Experimental (Value (..), from, innerJoin, on, select, table, val, where_, (:&) (..), (==.), (^.))
import Database.Persist.Postgresql (ConnectionString)
import Database.Persist.Sql (SqlPersistT)
import Database.Verb (runDataBaseWithOutLog)
import Scheme (EntityField (..), News (..), Password (..), User (..))
import Types (Login (..), PasswordUser (..), Title (..))

validCopyRight :: ConnectionString -> Login -> Title -> IO (Either SomeException Bool)
validCopyRight connString login title = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadFail m, MonadIO m) => SqlPersistT m Bool
    fetchAction = do
      logins <- fetchUserFromNews
      case logins of
        [Value loginNews] -> pure (getLogin login == loginNews)
        _ -> pure False -- noValid
      where
        fetchUserFromNews :: (MonadIO m) => SqlPersistT m [Value T.Text]
        fetchUserFromNews = select $ do
          (news :& user) <-
            from
              $ table @News
                `innerJoin` table @User
              `on` (\(n :& u) -> n ^. NewsUserId ==. (u ^. UserId))
          where_ (news ^. NewsTitle ==. (val . getTitle) title)
          pure (user ^. UserLogin)

validPassword :: ConnectionString -> Login -> PasswordUser -> IO (Either SomeException Bool)
validPassword connString login password = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadFail m, MonadIO m) => SqlPersistT m Bool
    fetchAction = do
      qpass <- fetchSaltAndPassword
      case qpass of
        [Value qpass'] -> pure $ Database.Crypto.validPassword (getPasswordUser password) qpass'
        _ -> pure False -- noValid
    fetchSaltAndPassword :: (MonadFail m, MonadIO m) => SqlPersistT m [Value T.Text]
    fetchSaltAndPassword = select $ do
      (user :& pass) <-
        from
          $ table @User
            `innerJoin` table @Password
          `on` (\(u :& p) -> u ^. UserPasswordId ==. p ^. PasswordId)
      where_ (user ^. UserLogin ==. (val . getLogin) login)
      pure (pass ^. PasswordQuasiPassword)
