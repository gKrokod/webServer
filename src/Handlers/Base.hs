module Handlers.Base where

import Data.List (intercalate)
import Scheme
import Handlers.Logger (Log(..), logMessage) 
import qualified Handlers.Logger 
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Int (Int64)
-- import Control.Exception (SomeException)
import Control.Exception (SomeException, displayException)
import Control.Monad (when)
import Data.Either (isLeft)

data Success = Put | Change | Get deriving Show
type Name = T.Text
type Login = T.Text
type Time = UTCTime
type PasswordUser = T.Text
type Label = T.Text
type NewLabel = T.Text
type NumberImage = Int64
type Header = T.Text
type Base64 = T.Text
type Title = T.Text
type Content = T.Text
type URI_Image = T.Text
type NewsOut = (Title, UTCTime, Login, [Label], Content, [URI_Image], Bool)
-- type KeyIdUser = Int64
-- type KeyIdCategory = Int64
data Handle m = Handle 
  {
--API
    logger :: Handlers.Logger.Handle m,
    panigate :: Int,
    getTime :: m (UTCTime),

    pullAllUsers :: m (Either SomeException [User]),
-- getAllNews :: (Monad m) => Handle m -> m (Either T.Text [NewsOut])
    pullAllNews :: m (Either SomeException [NewsOut]),
-- getAllNews :: (Monad m) => Handle m -> m (Either T.Text [NewsOut])
    pullAllCategories :: m (Either SomeException [Category]),
-- getAllCategories :: (Monad m) => Handle m -> m (Either T.Text [Category])
    pullImage :: NumberImage -> m (Either SomeException (Maybe Image)),

    findUserByLogin :: Login -> m (Maybe User), 
    findCategoryByLabel :: Label -> m (Maybe Category),
    findNewsByTitle :: Title -> m (Maybe News), 

    putUser :: Name -> Login -> PasswordUser -> UTCTime -> Bool -> Bool -> m (Either SomeException Success), 
-- createUserBase :: (Monad m) => Handle m -> Name -> Login -> PasswordUser -> Bool -> Bool -> m (Either T.Text Success)  
    putCategory :: Label -> Maybe Label -> m (Either SomeException Success), 
-- createCategoryBase :: (Monad m) => Handle m -> Label -> Maybe Label -> m (Either T.Text Success) 
    -- putNews :: Title -> UTCTime -> Login -> Label -> Content -> [Image] -> Bool -> m (),
    putNews :: Title -> UTCTime -> Login -> Label -> Content -> [Image] -> Bool -> m (Either SomeException Success),
-- createNewsBase :: (Monad m) => Handle m -> Title -> Login -> Label -> Content -> [Image] -> Bool -> m (Either T.Text Success) 

    editNews :: Title -> UTCTime -> Maybe Title -> Maybe Login -> Maybe Label -> Maybe Content -> [Image] -> Maybe Bool -> m (Either SomeException Success), 
-- updateNews :: (Monad m) => Handle m -> Title -> Maybe Title -> Maybe Login -> Maybe Label -> Maybe Content -> [Image] -> Maybe Bool -> m (Either T.Text Success)
    editCategory :: Label -> NewLabel -> Maybe Label -> m (Either SomeException Success)
-- updateCategory :: (Monad m) => Handle m -> Label -> NewLabel -> Maybe Label -> m (Either T.Text Success)  
  }

                -- when (isLeft tryCreate) (logMessage (logger h) Handlers.Logger.Error "Can't putUser")
                -- pure $ either (Left . T.pack . displayException) Right tryCreate 

getAllNews :: (Monad m) => Handle m -> m (Either T.Text [NewsOut])
getAllNews h = do
  logMessage (logger h) Debug ("Try to get all news from database")
  news <- pullAllNews h
  when (isLeft news) (logMessage (logger h) Handlers.Logger.Error "function pullAllNews fail")
  pure $ either (Left . T.pack . displayException) Right news 

getAllUsers :: (Monad m) => Handle m -> m (Either T.Text [User])
getAllUsers h = do
  logMessage (logger h) Debug ("Try to get all users from database")
  users <- pullAllUsers h
  when (isLeft users) (logMessage (logger h) Handlers.Logger.Error "function pullAllUsers fail")
  pure $ either (Left . T.pack . displayException) Right users 

getAllCategories :: (Monad m) => Handle m -> m (Either T.Text [Category])
getAllCategories h = do
  logMessage (logger h) Debug ("Try to get all categories from database")
  categories <- pullAllCategories h
  when (isLeft categories) (logMessage (logger h) Handlers.Logger.Error "function pullAllCategories fail")
  pure $ either (Left . T.pack . displayException) Right categories 

getImage :: (Monad m) => Handle m -> NumberImage -> m (Either T.Text Image)
getImage h uid = do
  logMessage (logger h) Debug ("Try to get image from database " <> (T.pack . show $ uid))
  image <- pullImage h uid 
  case image of
    Left e -> do 
      let e' = T.pack . show $ e
      logMessage (logger h) Handlers.Logger.Error e'  
      pure $ Left e' 
    Right Nothing -> do
      logMessage (logger h) Debug ("Image was not found in database")
      pure $ Left "Image was not found in database"
    Right (Just image) -> do
      logMessage (logger h) Debug ("Image was found in database")
      pure $ Right image 

updateNews :: (Monad m) => Handle m -> Title -> Maybe Title -> Maybe Login -> Maybe Label -> Maybe Content -> [Image] -> Maybe Bool -> m (Either T.Text Success)
updateNews h title newTitle newLogin newLabel newContent newImages newPublish = do
  logMessage (logger h) Debug ("Checks attributes for update news with title " <> title)
  existTitle <- maybe (Left "news don't exist") (\_ -> Right Change) <$> findNewsByTitle h title
  existNewTitle <- checkNews newTitle 
  existUser <- checkUser newLogin
  existCategory <- checkCategory newLabel
  case sequence_ [existTitle, existNewTitle, existUser, existCategory] of
    Left e -> do --todo   title : "News 5" \n newtitle: "Just News 51" so on
      logMessage (logger h) Warning  ("Fail to update news with attributes: " 
        <> (T.pack . show) title <> " " 
        <> (T.pack . show) newTitle <> " "  
        <> (T.pack . show) newLogin <> " "  
        <> (T.pack . show) newLabel <> " " ) 
      logMessage (logger h) Warning  e
      pure $ Left "fail to update news"
    Right _-> do
      logMessage (logger h) Debug  ("Ok. Updates news...")
      t <- getTime h
      tryEdit <- editNews h title t newTitle newLogin newLabel newContent newImages newPublish
      when (isLeft tryEdit) (logMessage (logger h) Handlers.Logger.Error "Can't editNews")
      pure $ either (Left . T.pack . displayException) Right tryEdit
  where
    -- checkUser :: (Monad m) => Maybe Login -> m (Either T.Text Success) -- todo WHY m1 not m ?
    checkUser Nothing = pure (Right Change)
    checkUser (Just login) = do
      user <- findUserByLogin h login
      case user of
        Nothing -> do
          logMessage (logger h) Warning  ("Fail update news. User don't exist! : " <> login)
          pure $ Left "Fail update news. User don't exist!"
        Just _ -> do
          logMessage (logger h) Debug  ("Ok. User exist! go ahead : " <> login)
          pure $ Right Change
      
    checkCategory Nothing = pure (Right Change)
    checkCategory (Just label) = do
      category <- findCategoryByLabel h label
      case category of
        Nothing -> do
          logMessage (logger h) Warning  ("Fail update news. Category don't exist! : " <> label)
          pure $ Left "Fail update news. Category don't exist!"
        Just _ -> do
          logMessage (logger h) Debug  ("Ok. Category exist! go ahead : " <> label)
          pure $ Right Change

    checkNews Nothing = pure (Right Change)
    checkNews (Just title) = do
      news <- findNewsByTitle h title
      case news of
        Nothing -> do
          logMessage (logger h) Debug  ("Ok. News don't exist! go ahead : " <> title)
          pure $ Right Change
        Just _ -> do
          logMessage (logger h) Warning  ("Fail update news. News with your title is existed! : " <> title)
          pure $ Left "Fail update news. News already exist!"

--   
         
createNewsBase :: (Monad m) => Handle m -> Title -> Login -> Label -> Content -> [Image] -> Bool -> m (Either T.Text Success) 
createNewsBase h title login label content images ispublish = do 
  logMessage (logger h) Debug ("Check news by title for create: " <> title)
  existTitle <- findNewsByTitle h title 
  logMessage (logger h) Debug ("Check user by login for create: " <> login)
  existUser <- findUserByLogin h login 
  logMessage (logger h) Debug ("Check category by label for create: " <> label)
  existCategory <- findCategoryByLabel h label 
  case (existTitle, existUser, existCategory) of
    (Nothing, Just _user, Just _category) -> do
      logMessage (logger h) Debug ("Create news with title, login and label: " <> title <> " " <> login <> " " <> label)
      time <- getTime h
      tryPut <- putNews h title time login label content images ispublish
      when (isLeft tryPut) (logMessage (logger h) Handlers.Logger.Error "Can't putNews")
      pure $ either (Left . T.pack . displayException) Right tryPut 
    _ -> do
      logMessage (logger h) Warning  ("Fail to create news with title, login and label: " <> title <> " " <> login <> " " <> label)
      pure $ Left "fail to create news"


updateCategory :: (Monad m) => Handle m -> Label -> NewLabel -> Maybe Label -> m (Either T.Text Success)  
-- todo
-- updateCategory :: (Monad m) => Handle m -> Label -> Maybe NewLabel -> Maybe Label -> m (Either T.Text Success)  
updateCategory h label newlabel parent = do
  logMessage (logger h) Debug ("Check category for label for update: " <> label <> " " <> newlabel)
  exist <- findCategoryByLabel h label
  existNew <- findCategoryByLabel h newlabel
  -- let flag = label == newlabel
  let existNew' = if (label == newlabel) then Nothing else existNew
  case (exist, existNew', parent) of
    (Just _, Nothing, Nothing) -> do
                                    logMessage (logger h) Debug ("Create category without parent and label: " <> label)
                                    tryEdit <- editCategory h label newlabel parent
                                    when (isLeft tryEdit) (logMessage (logger h) Handlers.Logger.Error "Can't editCategory")
                                    pure $ either (Left . T.pack . displayException) Right tryEdit
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
                                        tryEdit <- editCategory h label newlabel parent
                                        when (isLeft tryEdit) (logMessage (logger h) Handlers.Logger.Error "Can't editCategory")
                                        pure $ either (Left . T.pack . displayException) Right tryEdit
    _ -> do
          logMessage (logger h) Warning ("Abort. Category don't exist or .... Category: " <> label)
          pure $ Left "Category don't's exist or ..."

createCategoryBase :: (Monad m) => Handle m -> Label -> Maybe Label -> m (Either T.Text Success) 
createCategoryBase h label parent = do
  logMessage (logger h) Debug ("Check category for label for create: " <> label)
  exist <- findCategoryByLabel h label
  case (exist, parent) of
    (Just _, _) -> do
                logMessage (logger h) Warning ("Category arleady taken: " <> label)
                pure $ Left "Category arleady taken"
    (Nothing, Nothing) -> do
                logMessage (logger h) Debug ("Create category without parent and label: " <> label)
                tryPut <- putCategory h label parent
                when (isLeft tryPut) (logMessage (logger h) Handlers.Logger.Error "Can't putCategory")
                pure $ either (Left . T.pack . displayException) Right tryPut 
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
                    tryPut <- putCategory h label parent
                    when (isLeft tryPut) (logMessage (logger h) Handlers.Logger.Error "Can't putCategory")
                    pure $ either (Left . T.pack . displayException) Right tryPut 
    _ -> do
                logMessage (logger h) Warning ("fail for createCategory: ")
                pure $ Left "fail for create Category "


-- cat1 = Category {categoryLabel = "Abstract", categoryParent = Nothing }
-- cat3 = Category "Woman" (Just $ toSqlKey 1)
--
createUserBase :: (Monad m) => Handle m -> Name -> Login -> PasswordUser -> Bool -> Bool -> m (Either T.Text Success)  
createUserBase h name login pwd admin publish = do
  logMessage (logger h) Debug ("check user By login for  create: " <> login)
  exist <- findUserByLogin h login -- todo
  -- exist <- (pure Nothing ) -- findUserByLogin h login
  case exist of
    Just _ -> do
                logMessage (logger h) Warning ("Login arleady taken: " <> login)
                pure $ Left "Login arleady taken"
    Nothing-> do
                logMessage (logger h) Debug ("Create user...")
                -- makeHashPassword pwd
                let pwd' = pwd --for make QuasiPassowrd
                time <- getTime h
                tryCreate <- putUser h name login pwd' time admin publish 
                when (isLeft tryCreate) (logMessage (logger h) Handlers.Logger.Error "Can't putUser")
                pure $ either (Left . T.pack . displayException) Right tryCreate 

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
