module Handlers.Base (getCopyRight, getAllNews, updateNews, createNewsBase, updateCategoryBase, createCategoryBase, getAllCategories, getImage, createUserBase, getAllUsers, getResultValid, getPrivilege, Handle(..), NewsOut, Title, Content, NewLabel, Label, Login, PasswordUser, Name, NumberImage, Success(..), URI_Image) where

import Scheme  (Image(..), Category(..), User(..), IsValidPassword(..), News(..), ColumnType(..), Find(..), FilterItem(..), SortOrder(..))
import Handlers.Logger (Log(..), logMessage) 
import qualified Handlers.Logger 
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Int (Int64)
import Control.Exception (SomeException, displayException)
import Control.Monad (when)
import Data.Either (isLeft)
import Data.Bool (bool)

data Success = Put | Change | Get deriving Show
type Name = T.Text
type Login = T.Text
type PasswordUser = T.Text
type HashPasswordUser = T.Text
type Label = T.Text
type NewLabel = T.Text
type NumberImage = Int64
-- type Header = T.Text
-- type Base64 = T.Text
type Title = T.Text
type Content = T.Text
type URI_Image = T.Text
type Offset = Int
type Limit = Int
type NewsOut = (Title, UTCTime, Login, [Label], Content, [URI_Image], Bool)
data Handle m = Handle 
  {
--API
    logger :: Handlers.Logger.Handle m,
    userOffset :: Int, --default
    userLimit :: Int, -- default
    sortColumnNews :: ColumnType, -- default
    sortOrderNews :: SortOrder, -- default
    findSubString :: Maybe Find,
    filtersNews :: [FilterItem],
    --
    getTime :: m UTCTime,
    makeHashPassword :: PasswordUser -> UTCTime -> HashPasswordUser,
    validPassword :: Login -> PasswordUser -> m (Either SomeException Bool),
    validCopyRight :: Login -> Title-> m (Either SomeException Bool),
-- pullAllUsers :: Offset -> Limit -> m (Either SomeException [User])
    pullAllUsers :: Offset -> Limit -> m (Either SomeException [User]),
-- getAllNews :: (Monad m) => Handle m -> m (Either T.Text [NewsOut])
    pullAllNews :: Offset -> Limit -> ColumnType -> SortOrder -> Maybe Find -> [FilterItem] -> m (Either SomeException [NewsOut]),
-- getAllNews :: (Monad m) => Handle m -> m (Either T.Text [NewsOut])
    pullAllCategories :: Offset -> Limit -> m (Either SomeException [Category]),
-- getAllCategories :: (Monad m) => Handle m -> m (Either T.Text [Category])
    pullImage :: NumberImage -> m (Either SomeException (Maybe Image)),

    findUserByLogin :: Login -> m (Either SomeException (Maybe User)), 
    findCategoryByLabel :: Label -> m (Either SomeException (Maybe Category)),
    findNewsByTitle :: Title -> m (Either SomeException (Maybe News)), 

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
-- updateCategoryBase :: (Monad m) => Handle m -> Label -> NewLabel -> Maybe Label -> m (Either T.Text Success)  
  }

getCopyRight :: (Monad m) => Handle m -> Login -> Title ->  m (Either T.Text IsValidPassword)
getCopyRight h login title = do
  let logHandle = logger h
  logMessage logHandle Debug ("Check copyright for the login: " <> login <> " of the news with title: " <> title)
  tryValid <- validCopyRight h login title 
  when (isLeft tryValid) (logMessage logHandle Handlers.Logger.Error "function validCopyRight fail")
  pure $ either (Left . T.pack . displayException) (Right . bool NotValid Valid) tryValid 

getResultValid :: (Monad m) => Handle m -> Login -> PasswordUser ->  m (Either T.Text IsValidPassword)
getResultValid h login password = do
  let logHandle = logger h
  logMessage logHandle Debug ("Check password for: " <> login <> " " <> password)
  tryValid <- validPassword h login password 
  when (isLeft tryValid) (logMessage logHandle Handlers.Logger.Error "function validPassword fail")
  pure $ either (Left . T.pack . displayException) (Right . bool NotValid Valid) tryValid 

type IsAdmin = Bool
type IsPublisher = Bool

getPrivilege :: (Monad m) => Handle m -> Login -> m (Either T.Text (IsAdmin, IsPublisher))
getPrivilege h login = do
  let logHandle = logger h
  logMessage logHandle Debug ("Get privilege for login: " <> login)
  tryFindUser <- findUserByLogin h login
  when (isLeft tryFindUser) (logMessage logHandle Error "function findUserByLogin fail")
  case tryFindUser of
    Left e -> pure . Left . T.pack . displayException $ e
    --todo spisok novostej
    Right (Just (User _ _ _ _ a p)) -> do
      logMessage logHandle Debug (T.pack $ "Privilege: Admin " <> show a <> " Publisher " <> show p)
      pure $ Right (a, p)
    _ -> do -- Right Nothing - not found in base
      logMessage logHandle Debug "Privilege: Admin False False "
      pure $ Right (False, False)
  --
getAllNews :: (Monad m) => Handle m -> m (Either T.Text [NewsOut])
getAllNews h = do
  let logHandle = logger h
  logMessage logHandle Debug "Try to get all news from database"
  news <- pullAllNews h (userOffset h) (userLimit h) (sortColumnNews h) (sortOrderNews h) (findSubString h) (filtersNews h)
  when (isLeft news) (logMessage logHandle Handlers.Logger.Error "function pullAllNews fail")
  pure $ either (Left . T.pack . displayException) Right news 

getAllUsers :: (Monad m) => Handle m -> m (Either T.Text [User])
getAllUsers h = do
  let logHandle = logger h
  logMessage logHandle Debug "Try to get all users from database"
  users <- pullAllUsers h (userOffset h) (userLimit h)
  when (isLeft users) (logMessage logHandle Handlers.Logger.Error "function pullAllUsers fail")
  pure $ either (Left . T.pack . displayException) Right users 

getAllCategories :: (Monad m) => Handle m -> m (Either T.Text [Category])
getAllCategories h = do
  let logHandle = logger h
  logMessage logHandle Debug "Try to get all categories from database"
  categories <- pullAllCategories h (userOffset h) (userLimit h)
  when (isLeft categories) (logMessage logHandle Handlers.Logger.Error "function pullAllCategories fail")
  pure $ either (Left . T.pack . displayException) Right categories 

getImage :: (Monad m) => Handle m -> NumberImage -> m (Either T.Text Image)
getImage h uid = do
  let logHandle = logger h
  logMessage logHandle Debug ("Try to get image from database " <> (T.pack . show $ uid))
  images <- pullImage h uid 
  case images of
    Left e -> do 
      logMessage logHandle Handlers.Logger.Error "function pullImage fail"
      pure . Left . T.pack . show $ e
    Right Nothing -> do
      logMessage logHandle Debug "Image was not found in database"
      pure $ Left "Image was not found in database"
    Right (Just image) -> do
      logMessage logHandle Debug "Image was found in database"
      pure $ Right image 

updateNews :: (Monad m) => Handle m -> Title -> Maybe Title -> Maybe Login -> Maybe Label -> Maybe Content -> [Image] -> Maybe Bool -> m (Either T.Text Success)
updateNews h title newTitle newLogin newLabel newContent newImages newPublish = do
  let logHandle = logger h
  logMessage logHandle Debug ("Checks attributes for update news with title " <> title)
  existTitle <- either (Left . T.pack . displayException) 
                       (maybe (Left "news don't exist") (\_ -> Right Change)) 
                       <$> findNewsByTitle h title
  existNewTitle <- checkNews newTitle 
  existUser <- checkUser newLogin
  existCategory <- checkCategory newLabel

  case sequence_ [existTitle, existNewTitle, existUser, existCategory] of
    Left e -> do --todo   title : "News 5" \n newtitle: "Just News 51" so on
      logMessage logHandle Warning  ("Fail to update news with attributes: " 
        <> (T.pack . show) title <> " " 
        <> (T.pack . show) newTitle <> " "  
        <> (T.pack . show) newLogin <> " "  
        <> (T.pack . show) newLabel <> " " ) 
      logMessage logHandle Warning  e
      pure $ Left "fail to update news"
    Right _-> do
      logMessage logHandle Debug  "Ok. Updates news..."
      t <- getTime h
      tryEdit <- editNews h title t newTitle newLogin newLabel newContent newImages newPublish
      when (isLeft tryEdit) (logMessage logHandle Handlers.Logger.Error "Can't editNews")
      pure $ either (Left . T.pack . displayException) Right tryEdit
  where
    -- checkUser :: (Monad m) => Maybe Login -> m (Either T.Text Success) -- todo WHY m1 not m ?
    checkUser Nothing = pure $ Right Change
    checkUser (Just login) = do
      tryFindUser <- findUserByLogin h login
      when (isLeft tryFindUser) (logMessage (logger h) Error  "function findUserByLogin fail")
      pure (either (Left . T.pack . displayException)
           (maybe  (Left "Fail update news. User don't exist!") (\_ -> Right Change)) 
           tryFindUser )
      --
    checkCategory Nothing = pure (Right Change)
    checkCategory (Just label) = do
      tryFindCategory <- findCategoryByLabel h label
      when (isLeft tryFindCategory) (logMessage (logger h) Error  "function findCategoryByLabel fail")
      pure (either (Left . T.pack . displayException)
           (maybe  (Left "Fail update news. Category don't exist!") (\_ -> Right Change)) 
           tryFindCategory )

    checkNews Nothing = pure (Right Change)
    checkNews (Just titleNews) = do
      tryFindNews <- findNewsByTitle h titleNews
      when (isLeft tryFindNews) (logMessage (logger h) Error  "function findNewsByTitle fail")
      pure (either (Left . T.pack . displayException)
           (maybe  (Right Change)  (\_ -> Left $ "Fail update news. News with your title is existed! : " <> titleNews))
           tryFindNews )
--   
         

createNewsBase :: (Monad m) => Handle m -> Title -> Login -> Label -> Content -> [Image] -> Bool -> m (Either T.Text Success) 
createNewsBase h title login label content images ispublish = do 
  let logHandle = logger h
  logMessage logHandle Debug ("Check news by title for create: " <> title)
  existTitle <- findNewsByTitle h title 
  when (isLeft existTitle) (logMessage logHandle Handlers.Logger.Error "function findNewsByTitle fail")

  logMessage logHandle Debug ("Check user by login for create: " <> login)
  existUser <- findUserByLogin h login 
  when (isLeft existUser) (logMessage logHandle Handlers.Logger.Error "function findUserByLogin fail")

  logMessage logHandle Debug ("Check category by label for create: " <> label)
  existCategory <- findCategoryByLabel h label 
  when (isLeft existCategory) (logMessage logHandle Handlers.Logger.Error "function findCategoryByLabel fail")

  case (existTitle, existUser, existCategory) of
    (Right Nothing, Right (Just _user), Right (Just _category)) -> do
      logMessage logHandle Debug ("Create news with title, login and label: " <> title <> " " <> login <> " " <> label)
      time <- getTime h
      tryPut <- putNews h title time login label content images ispublish
      when (isLeft tryPut) (logMessage logHandle Handlers.Logger.Error "function putNews fail")
      pure $ either (Left . T.pack . displayException) Right tryPut 
    _ -> do
      logMessage logHandle Warning  ("Fail to create news with title, login and label: " <> title <> " " <> login <> " " <> label)
      pure $ Left "fail to create news"


updateCategoryBase :: (Monad m) => Handle m -> Label -> NewLabel -> Maybe Label -> m (Either T.Text Success)  
updateCategoryBase h label newlabel parent = do
  let logHandle = logger h
  logMessage logHandle Debug ("Check category for label for update: " <> label <> " " <> newlabel)

  exist <- findCategoryByLabel h label
  when (isLeft exist) (logMessage logHandle Handlers.Logger.Error "function findCategoryByLabel fail")

  existNew <- findCategoryByLabel h newlabel
  when (isLeft existNew) (logMessage logHandle Handlers.Logger.Error "function findCategoryByLabel fail")
  -- let flag = label == newlabel
  let existNew' = if label == newlabel then Right Nothing else existNew
  case (sequence [exist, existNew'], parent) of
    (Left e, _) -> pure . Left . T.pack . displayException $ e
    (Right [Just _, Nothing] , Nothing) -> do
                                    logMessage logHandle Debug ("Create category without parent and label: " <> label)
                                    tryEdit <- editCategory h label newlabel parent
                                    when (isLeft tryEdit) (logMessage logHandle Handlers.Logger.Error "Can't editCategory")
                                    pure $ either (Left . T.pack . displayException) Right tryEdit
    (Right [Just _, Nothing], Just labelParent) -> do
                                    logMessage logHandle Debug ("Update category: " <> label)
                                    logMessage logHandle Debug ("Check parent for category. Parent: " <> labelParent)
                                    existLabel <- findCategoryByLabel h labelParent
                                    case existLabel of
                                      Left e -> do
                                        logMessage logHandle Handlers.Logger.Error "function findCategoryByLabel fail"
                                        pure . Left . T.pack . displayException $ e 
                                      Right Nothing -> do
                                        logMessage logHandle Warning ("Abort. Parent don't exist: " <> labelParent)
                                        pure $ Left "Parent dont' exist"
                                      Right (Just _) -> do
                                        logMessage logHandle Debug "Parent exist"
                                        tryEdit <- editCategory h label newlabel parent
                                        when (isLeft tryEdit) (logMessage logHandle Handlers.Logger.Error "Can't editCategory")
                                        pure $ either (Left . T.pack . displayException) Right tryEdit
    _ -> do
          logMessage logHandle Warning ("Abort. Category don't exist or .... Category: " <> label)
          pure $ Left "Category don't's exist or ..."

createCategoryBase :: (Monad m) => Handle m -> Label -> Maybe Label -> m (Either T.Text Success) 
createCategoryBase h label parent = do
  let logHandle = logger h
  logMessage logHandle Debug ("Check category for label for create: " <> label)
  exist <- findCategoryByLabel h label
  case (exist, parent) of
    (Left e, _) -> do
                logMessage logHandle Error "function findCategoryByLabel fail"
                pure . Left . T.pack . displayException $ e
    (Right (Just _), _) -> do
                logMessage logHandle Warning ("Category arleady taken: " <> label)
                pure $ Left "Category arleady taken"
    (Right Nothing, Nothing) -> do
                logMessage logHandle Debug ("Create category without parent and label: " <> label)
                tryPut <- putCategory h label parent
                when (isLeft tryPut) (logMessage logHandle Handlers.Logger.Error "function putCategory fali")
                pure $ either (Left . T.pack . displayException) Right tryPut 
    (Right Nothing, Just labelParent) -> do
                logMessage logHandle Debug ("Create category with parent and label: " <> labelParent <> " " <> label)
                logMessage logHandle Debug ("Check parent: " <> labelParent)
                existLabel <- findCategoryByLabel h labelParent
                case existLabel of
                  Left e -> do
                    logMessage logHandle Error "function findCategoryByLabel fail"
                    pure . Left . T.pack . displayException $ e
                  Right Nothing -> do
                    logMessage logHandle Warning ("Abort. Parent dont' exist: " <> labelParent)
                    pure $ Left "Parent dont' exist"
                  _ -> do
                    logMessage logHandle Debug "Parent exist"
                    tryPut <- putCategory h label parent
                    when (isLeft tryPut) (logMessage logHandle Handlers.Logger.Error "function putCategory fail")
                    pure $ either (Left . T.pack . displayException) Right tryPut 
--
createUserBase :: (Monad m) => Handle m -> Name -> Login -> PasswordUser -> Bool -> Bool -> m (Either T.Text Success)  
createUserBase h name login pwd admin publish = do
  let logHandle = logger h
  logMessage logHandle Debug ("check user By login for  create: " <> login)
  tryFind <- findUserByLogin h login -- todo
  case tryFind of
    Left e -> do
                logMessage logHandle Error "function findUserByLogin fail"
                pure . Left . T.pack . displayException $ e 
    Right (Just _) -> do
                logMessage logHandle Warning ("Login arleady taken: " <> login)
                pure $ Left "Login arleady taken"
    Right Nothing-> do
                logMessage logHandle Debug "Create user..."
                time <- getTime h
                --- crypto
                let pwd' = makeHashPassword h pwd time --for make QuasiPassowrd
                tryCreate <- putUser h name login pwd' time admin publish 
                when (isLeft tryCreate) (logMessage logHandle Handlers.Logger.Error "Can't putUser")
                pure $ either (Left . T.pack . displayException) Right tryCreate 
