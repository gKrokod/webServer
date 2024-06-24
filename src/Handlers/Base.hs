module Handlers.Base where

import Scheme
import Handlers.Logger (Log(..), logMessage) 
import qualified Handlers.Logger 
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Int (Int64)
type Name = T.Text
type Login = T.Text
type Time = UTCTime
type PasswordUser = T.Text
data Success = Put | Change | Get deriving Show
type Label = T.Text
type NewLabel = T.Text
type NumberImage = Int64
type Header = T.Text
type Base64 = T.Text
type Title = T.Text
type Content = T.Text
type KeyIdUser = Int64
type KeyIdCategory = Int64

data Handle m = Handle 
  {
    logger :: Handlers.Logger.Handle m,
    panigate :: Int,
-- api user: create +, getall (getAllUsers) +
    putUser :: Name -> Login -> PasswordUser -> UTCTime -> Bool -> Bool -> m (), 
    findUserByLogin :: Login -> m (Maybe User), 
    getTime :: m (UTCTime),
    getAllUsers :: m [User],
-- api category: create +, getall (getAllCategories) +, edit +      add getBranchCategories for news api
    putCategory :: Label -> Maybe Label -> m (), 
    changeCategory :: Label -> NewLabel -> Maybe Label -> m (), 
    getAllCategories :: m [Category],
    getBranchCategories :: Label -> m [Category],
    findCategoryByLabel :: Label -> m (Maybe Category),
-- api imagy: getOne
    getImage :: NumberImage -> m (Maybe Image),
    deleteImagesFromBank :: Title -> m (),
    putImage :: Header -> Base64 -> m (), 
-- api news: create +, getAllNews +. edit -
    putNews :: Title -> UTCTime -> Login -> Label -> Content -> [Image] -> Bool -> m (),
    editNews :: Title -> UTCTime -> Maybe Title -> Maybe Login -> Maybe Label -> Maybe Content -> [Image] -> Maybe Bool -> m (), 
    getAllNews :: m [News],
    findNewsByTitle :: Title -> m (Maybe News) 
    --
-- add some func
  }

replaceField :: a -> Maybe a -> a
replaceField _ (Just a) = a
replaceField a _ = a

--  News sql=news
--   title T.Text
--   created UTCTime
--   userId UserId
--   categoryId CategoryId
--   content T.Text
--   isPublish Bool
--   UniqueNews title
--   deriving Eq Show
--
-- updateNews :: (Monad m) => Handle m -> Title -> Maybe Title -> Maybe Login -> Maybe Label -> Maybe Content -> [Image] -> Maybe Bool -> m (Either T.Text Success)
-- updateNews h title login label content [] ispublish = undefined
-- updateNews h title login label content xs ispublish = do
--   logMessage (logger h) Debug ("Clear ImageBank for news : " <> title)
--   deleteImagesFromBank title
--   
-- todo delete from imageBase link on images
-- upd
         
createNews :: (Monad m) => Handle m -> Title -> Login -> Label -> Content -> [Image] -> Bool -> m (Either T.Text Success) 
createNews h title login label content images ispublish = do 
  logMessage (logger h) Debug ("Check news by title for create: " <> title)
  existTitle <- findNewsByTitle h title 
  logMessage (logger h) Debug ("Check user by login for create: " <> login)
  existUser <- findUserByLogin h login 
  logMessage (logger h) Debug ("Check category by label for create: " <> label)
  existCategory <- findCategoryByLabel h label 
  -- esli kartinki yze est, ne nado ix vstavlyat
  -- todo poisk kartinok po uniue sochetaniu
  case (existTitle, existUser, existCategory) of
    (Nothing, Just user, Just category) -> do
      logMessage (logger h) Debug ("Create news with title, login and label: " <> title <> " " <> login <> " " <> label)
      time <- getTime h
      putNews h title time login label content images ispublish
      pure $ Right Put
    _ -> do
      logMessage (logger h) Warning  ("Fail to create news with title, login and label: " <> title <> " " <> login <> " " <> label)
      pure $ Left "fail to create news"


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
                                        logMessage (logger h) Warning ("Abort. Parent don't exist: " <> labelParent)
                                        pure $ Left "Parent dont' exist"
                                      _ -> do
                                        logMessage (logger h) Debug ("Parent exist")
                                        changeCategory h label newlabel parent
                                        pure $ Right Change 
    _ -> do
          logMessage (logger h) Warning ("Abort. Category don't exist or .... Category: " <> label)
          pure $ Left "Category don't's exist or ..."
          

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
