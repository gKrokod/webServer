module Handlers.Base where

import Scheme
import Handlers.Logger (Log(..), logMessage) 
import qualified Handlers.Logger 
import qualified Data.Text as T
import Data.Time (UTCTime)

type Name = T.Text
type Login = T.Text
type Time = UTCTime
type PasswordUser = T.Text
data Success = Put | Get deriving Show

data Handle m = Handle 
  {
    logger :: Handlers.Logger.Handle m,
    putUser :: Name -> Login -> PasswordUser -> UTCTime -> Bool -> Bool -> m (), 
    findUserByLogin :: Login -> m (Maybe User), 
    getTime :: m (UTCTime),
    getAllUsers :: m [User]
    -- add some func
  }


createUser :: (Monad m) => Handle m -> Name -> Login -> PasswordUser -> Bool -> Bool -> m (Either T.Text Success)  
createUser h name login pwd admin publish = do
  exist <- findUserByLogin h login
  case exist of
    Just _ -> do
                logMessage (logger h) Warning ("Login arleady taken: " <> login)
                pure $ Left "Login arleady taken"
    Nothing-> do
                logMessage (logger h) Debug ("Create user...")
                -- makeHashPassword pwd
                let pwd' = pwd
                time <- getTime h
                putUser h name login pwd' time admin publish 
                pure $ Right Put 


getUser :: (Monad m) => Handle m -> Login -> m (Maybe User)
getUser h login = do
  logMessage (logger h) Debug ("get User: " <> login)
  xs <- getAllUsers h
  let helper :: [User] -> Login -> Maybe User
      helper (x : xs) login | userLogin x == login = Just x
                            | otherwise = helper xs login
      helper [] _ = Nothing
  pure (helper xs login)

checkPassword :: (Monad m) => Handle m -> Login -> PasswordUser -> m (Bool)
checkPassword h login pass = do
  logMessage (logger h) Debug ("check password for User: " <> login)
  mbUser <- getUser h login
  pure $ case mbUser of
           Nothing -> False
           Just user -> pass == undefined (userPasswordId user)  -- tyt nyzen poisk po tablise passwordod
                           -- :: PasswordIdUser -> PasswordUser

--
-- data Handle m = Handle {
--   updateUser :: User -> m (),
--   updateNews :: News -> m (),
--   takeUsers :: m Users,
--   takeNews :: m [News],
--   takeCategories :: m CategoryDictionary,
--   updateCategories :: CategoryDictionary -> m (),
--   findImage :: Int -> m (Maybe Image),
--   logger :: Handlers.Logger.Handle m
--                        }
--  User sql=users
--   name T.Text
--   login T.Text
--   passwordId PasswordId
--   created UTCTime
--   isAdmin Bool
--   isPublisher Bool
--   UniqueUser login
--   deriving Eq Show
--  Password sql=passwords
--    quasiPassword T.Text
--    deriving Eq Show
