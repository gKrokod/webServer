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
data Success = Put | Change | Get deriving Show
type Label = T.Text
type NewLabel = T.Text

data Handle m = Handle 
  {
    logger :: Handlers.Logger.Handle m,
    panigate :: Int,
    putUser :: Name -> Login -> PasswordUser -> UTCTime -> Bool -> Bool -> m (), 
    findUserByLogin :: Login -> m (Maybe User), 
    getTime :: m (UTCTime),
    getAllUsers :: m [User],

    putCategory :: Label -> Maybe Label -> m (), 
    changeCategory :: Label -> NewLabel -> Maybe Label -> m (), 
    getAllCategories :: m [Category],
    getBranchCategories :: Label -> m [Category],
    findCategoryByLabel :: Label -> m (Maybe Category)
    -- add some func
  }

updateCategory :: (Monad m) => Handle m -> Label -> NewLabel -> Maybe Label -> m (Either T.Text Success)  
updateCategory h label newlabel parent = do
  logMessage (logger h) Debug ("Check category for label for update: " <> label <> " " <> newlabel)
  exist <- findCategoryByLabel h label
  existNew <- findCategoryByLabel h newlabel
  -- let flag = label == newlabel
  let existNew' = if (label == newlabel) then Nothing else existNew
  case (exist, existNew', parent) of
    (Just _, Nothing, Nothing) -> do
                                    logMessage (logger h) Debug ("Create category without parent and label: " <> label)
                                    changeCategory h label newlabel parent
                                    pure $ Right Change 
    (Just _, Nothing, Just labelParent) -> do
                                    logMessage (logger h) Debug ("Update category: " <> label)
                                    logMessage (logger h) Debug ("Check parent for category. Parent: " <> labelParent)
                                    exist <- findCategoryByLabel h labelParent
                                    case exist of
                                      Nothing -> do
                                        logMessage (logger h) Warning ("Abort. Parent dont' exist: " <> labelParent)
                                        pure $ Left "Parent dont' exist"
                                      _ -> do
                                        logMessage (logger h) Debug ("Parent exist")
                                        changeCategory h label newlabel parent
                                        pure $ Right Change 
    _ -> do
          logMessage (logger h) Warning ("Abort. Category don't exist or .... Category: " <> label)
          pure $ Left "Category dont' exist or ..."
          

createCategory :: (Monad m) => Handle m -> Label -> Maybe Label -> m (Either T.Text Success) 
createCategory h label parent = do
  logMessage (logger h) Debug ("Check category for label for create: " <> label)
  exist <- findCategoryByLabel h label
  case (exist, parent) of
    (Just _, _) -> do
                logMessage (logger h) Warning ("Category arleady taken: " <> label)
                pure $ Left "Category arleady taken"
    (Nothing, Nothing) -> do
                logMessage (logger h) Debug ("Create category without parent and label: " <> label)
                putCategory h label parent
                pure $ Right Put 
    (Nothing, Just labelParent) -> do
                logMessage (logger h) Debug ("Create category with parent and label: " <> labelParent <> " " <> label)
                logMessage (logger h) Debug ("Check parent: " <> labelParent)
                exist <- findCategoryByLabel h labelParent
                case exist of
                  Nothing -> do
                    logMessage (logger h) Warning ("Abort. Parent dont' exist: " <> labelParent)
                    pure $ Left "Parent dont' exist"
                  _ -> do
                    logMessage (logger h) Debug ("Parent exist")
                    putCategory h label parent 
                    pure $ Right Put 
    _ -> do
                logMessage (logger h) Warning ("fail for createCategory: ")
                pure $ Left "fail for create Category "


-- cat1 = Category {categoryLabel = "Abstract", categoryParent = Nothing }
-- cat3 = Category "Woman" (Just $ toSqlKey 1)
--
createUser :: (Monad m) => Handle m -> Name -> Login -> PasswordUser -> Bool -> Bool -> m (Either T.Text Success)  
createUser h name login pwd admin publish = do
  logMessage (logger h) Debug ("check user By login for  create: " <> login)
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


-- checkPassword :: (Monad m) => Handle m -> Login -> PasswordUser -> m (Bool)
-- checkPassword h login pass = do
--   logMessage (logger h) Debug ("check password for User: " <> login)
--   mbUser <- getUser h login
--   pure $ case mbUser of
--            Nothing -> False
--            Just user -> pass == undefined (userPasswordId user)  -- tyt nyzen poisk po tablise passwordod
--                            -- :: PasswordIdUser -> PasswordUser

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
