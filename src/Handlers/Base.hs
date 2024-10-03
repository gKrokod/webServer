{-# LANGUAGE RecordWildCards #-}

module Handlers.Base (getCopyRight, getAllNews, updateNews, createNewsBase, updateCategoryBase, createCategoryBase, getAllCategories, getImage, createUserBase, getAllUsers, getResultValid, getPrivilege, Handle (..), NewsOut, Title, Content, NewLabel, Label, Login, PasswordUser, Name, NumberImage, Success (..), URI_Image, Offset (..), Limit (..)) where

import Control.Exception (SomeException, displayException)
import Control.Monad (when)
import Data.Bool (bool)
import Data.Either (isLeft)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Handlers.Logger (Log (..), logMessage)
import qualified Handlers.Logger
import Scheme (Category (..), ColumnType (..), FilterItem (..), Find (..), Image (..), IsValidPassword (..), News (..), SortOrder (..), User (..))
import Types (CategoryInternal (..), Content (..), Label (..), Login (..), Name (..), NewLabel, NewsEditInternal (..), NewsInternal (..), NewsOut (..), NumberImage (..), PasswordUser (..), Title (..), URI_Image (..), UserInternal (..))

data Success = Put | Change | Get deriving (Show, Eq)

newtype Offset = MkOffset {getOffset :: Int}

newtype Limit = MkLimit {getLimit :: Int}

type HashPasswordUser = T.Text

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    userOffset :: Int, -- default
    userLimit :: Int, -- default
    sortColumnNews :: ColumnType, -- default
    sortOrderNews :: SortOrder, -- default
    findSubString :: Maybe Find,
    filtersNews :: [FilterItem],
    --
    getTime :: m UTCTime,
    makeHashPassword :: PasswordUser -> UTCTime -> HashPasswordUser,
    validPassword :: Login -> PasswordUser -> m (Either SomeException Bool),
    validCopyRight :: Login -> Title -> m (Either SomeException Bool),
    pullAllUsers :: Offset -> Limit -> m (Either SomeException [User]),
    pullAllNews :: Offset -> Limit -> ColumnType -> SortOrder -> Maybe Find -> [FilterItem] -> m (Either SomeException [NewsOut]),
    pullAllCategories :: Offset -> Limit -> m (Either SomeException [Category]),
    pullImage :: NumberImage -> m (Either SomeException (Maybe Image)),
    findUserByLogin :: Login -> m (Either SomeException (Maybe User)),
    findCategoryByLabel :: Label -> m (Either SomeException (Maybe Category)),
    findNewsByTitle :: Title -> m (Either SomeException (Maybe News)),
    putUser :: UserInternal -> UTCTime -> m (Either SomeException Success),
    putCategory :: CategoryInternal -> m (Either SomeException Success),
    putNews :: NewsInternal -> UTCTime -> m (Either SomeException Success),
    editNews :: Title -> UTCTime -> NewsEditInternal -> m (Either SomeException Success),
    editCategory :: Label -> CategoryInternal -> m (Either SomeException Success)
  }

getCopyRight :: (Monad m) => Handle m -> Login -> Title -> m (Either T.Text IsValidPassword)
getCopyRight h login title = do
  let logHandle = logger h
  logMessage logHandle Debug ("Check copyright for the login: " <> getLogin login <> " of the news with title: " <> getTitle title)
  tryValid <- validCopyRight h login title
  when (isLeft tryValid) (logMessage logHandle Handlers.Logger.Error "function validCopyRight fail")
  pure $ either (Left . T.pack . displayException) (Right . bool NotValid Valid) tryValid

getResultValid :: (Monad m) => Handle m -> Login -> PasswordUser -> m (Either T.Text IsValidPassword)
getResultValid h login password = do
  let logHandle = logger h
  logMessage logHandle Debug ("Check password for: " <> getLogin login <> " " <> getPasswordUser password)
  tryValid <- validPassword h login password
  when (isLeft tryValid) (logMessage logHandle Handlers.Logger.Error "function validPassword fail")
  pure $ either (Left . T.pack . displayException) (Right . bool NotValid Valid) tryValid

type IsAdmin = Bool

type IsPublisher = Bool

getPrivilege :: (Monad m) => Handle m -> Login -> m (Either T.Text (IsAdmin, IsPublisher))
getPrivilege h login = do
  let logHandle = logger h
  logMessage logHandle Debug ("Get privilege for login: " <> getLogin login)
  tryFindUser <- findUserByLogin h login
  when (isLeft tryFindUser) (logMessage logHandle Error "function findUserByLogin fail")
  case tryFindUser of
    Left e -> pure . Left . T.pack . displayException $ e
    Right (Just (User _ _ _ _ a p)) -> do
      logMessage logHandle Debug (T.pack $ "Privilege: Admin " <> show a <> " Publisher " <> show p)
      pure $ Right (a, p)
    _ -> do
      logMessage logHandle Debug "Privilege: Admin False False "
      pure $ Right (False, False)

--
getAllNews :: (Monad m) => Handle m -> m (Either T.Text [NewsOut])
getAllNews h = do
  let logHandle = logger h
  logMessage logHandle Debug "Try to get all news from database"
  news <- pullAllNews h (MkOffset . userOffset $ h) (MkLimit . userLimit $ h) (sortColumnNews h) (sortOrderNews h) (findSubString h) (filtersNews h)
  when (isLeft news) (logMessage logHandle Handlers.Logger.Error "function pullAllNews fail")
  pure $ either (Left . T.pack . displayException) Right news

getAllUsers :: (Monad m) => Handle m -> m (Either T.Text [User])
getAllUsers h = do
  let logHandle = logger h
  logMessage logHandle Debug "Try to get all users from database"
  users <- pullAllUsers h (MkOffset . userOffset $ h) (MkLimit . userLimit $ h)
  when (isLeft users) (logMessage logHandle Handlers.Logger.Error "function pullAllUsers fail")
  pure $ either (Left . T.pack . displayException) Right users

getAllCategories :: (Monad m) => Handle m -> m (Either T.Text [Category])
getAllCategories h = do
  let logHandle = logger h
  logMessage logHandle Debug "Try to get all categories from database"
  categories <- pullAllCategories h (MkOffset . userOffset $ h) (MkLimit . userLimit $ h)
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

updateNews :: (Monad m) => Handle m -> Title -> NewsEditInternal -> m (Either T.Text Success)
updateNews h title newsEdit@(NewsEditInternal {..}) = do
  let logHandle = logger h
  logMessage logHandle Debug ("Checks attributes for update news with title " <> getTitle title)
  existTitle <-
    either
      (Left . T.pack . displayException)
      (maybe (Left "news don't exist") (\_ -> Right Change))
      <$> findNewsByTitle h title
  existNewTitle <- checkNews titleEditNews
  existUser <- checkUser authorEditNews
  existCategory <- checkCategory labelEditNews

  case sequence_ [existTitle, existNewTitle, existUser, existCategory] of
    Left e -> do
      logMessage
        logHandle
        Warning
        ( "Fail to update news with attributes: "
            <> (T.pack . show) title
            <> " "
            <> (T.pack . show) titleEditNews
            <> " "
            <> (T.pack . show) authorEditNews
            <> " "
            <> (T.pack . show) labelEditNews
            <> " "
        )
      logMessage logHandle Warning e
      pure $ Left "fail to update news"
    Right _ -> do
      logMessage logHandle Debug "Ok. Updates news..."
      t <- getTime h
      tryEdit <- editNews h title t newsEdit
      when (isLeft tryEdit) (logMessage logHandle Handlers.Logger.Error "Can't editNews")
      pure $ either (Left . T.pack . displayException) Right tryEdit
  where
    checkUser Nothing = pure $ Right Change
    checkUser (Just login) = do
      tryFindUser <- findUserByLogin h login
      when (isLeft tryFindUser) (logMessage (logger h) Error "function findUserByLogin fail")
      pure
        ( either
            (Left . T.pack . displayException)
            (maybe (Left "Fail update news. User don't exist!") (\_ -> Right Change))
            tryFindUser
        )
    --
    checkCategory Nothing = pure (Right Change)
    checkCategory (Just label) = do
      tryFindCategory <- findCategoryByLabel h label
      when (isLeft tryFindCategory) (logMessage (logger h) Error "function findCategoryByLabel fail")
      pure
        ( either
            (Left . T.pack . displayException)
            (maybe (Left "Fail update news. Category don't exist!") (\_ -> Right Change))
            tryFindCategory
        )

    checkNews Nothing = pure (Right Change)
    checkNews (Just titleNews) = do
      tryFindNews <- findNewsByTitle h titleNews
      when (isLeft tryFindNews) (logMessage (logger h) Error "function findNewsByTitle fail")
      pure
        ( either
            (Left . T.pack . displayException)
            (maybe (Right Change) (\_ -> Left $ "Fail update news. News with your title is existed! : " <> getTitle titleNews))
            tryFindNews
        )

--                                  }
createNewsBase :: (Monad m) => Handle m -> NewsInternal -> m (Either T.Text Success)
createNewsBase h news@(NewsInternal {..}) = do
  let logHandle = logger h
  logMessage logHandle Debug ("Check news by title for create: " <> getTitle titleNews)
  existTitle <- findNewsByTitle h titleNews
  when (isLeft existTitle) (logMessage logHandle Handlers.Logger.Error "function findNewsByTitle fail")

  logMessage logHandle Debug ("Check user by login for create: " <> getLogin authorNews)
  existUser <- findUserByLogin h authorNews
  when (isLeft existUser) (logMessage logHandle Handlers.Logger.Error "function findUserByLogin fail")

  logMessage logHandle Debug ("Check category by label for create: " <> getLabel labelNews)
  existCategory <- findCategoryByLabel h labelNews
  when (isLeft existCategory) (logMessage logHandle Handlers.Logger.Error "function findCategoryByLabel fail")

  case (existTitle, existUser, existCategory) of
    (Right Nothing, Right (Just _user), Right (Just _category)) -> do
      logMessage logHandle Debug ("Create news with title, login and label: " <> getTitle titleNews <> " " <> getLogin authorNews <> " " <> getLabel labelNews)
      time <- getTime h
      tryPut <- putNews h news time
      when (isLeft tryPut) (logMessage logHandle Handlers.Logger.Error "function putNews fail")
      pure $ either (Left . T.pack . displayException) Right tryPut
    _ -> do
      logMessage logHandle Warning ("Fail to create news with title, login and label: " <> getTitle titleNews <> " " <> getLogin authorNews <> " " <> getLabel labelNews)
      pure $ Left "fail to create news"

updateCategoryBase :: (Monad m) => Handle m -> Label -> CategoryInternal -> m (Either T.Text Success)
updateCategoryBase h label newCat@(CategoryInternal newlabel parent) = do
  let logHandle = logger h
  logMessage logHandle Debug ("Check category for label for update: " <> getLabel label <> " " <> getLabel newlabel)

  exist <- findCategoryByLabel h label
  when (isLeft exist) (logMessage logHandle Handlers.Logger.Error "function findCategoryByLabel fail")

  existNew <- findCategoryByLabel h newlabel
  when (isLeft existNew) (logMessage logHandle Handlers.Logger.Error "function findCategoryByLabel fail")
  let existNew' = if label == newlabel then Right Nothing else existNew
  case (sequence [exist, existNew'], parent) of
    (Left e, _) -> pure . Left . T.pack . displayException $ e
    (Right [Just _, Nothing], Nothing) -> do
      logMessage logHandle Debug ("Create category without parent and label: " <> getLabel label)
      tryEdit <- editCategory h label newCat
      when (isLeft tryEdit) (logMessage logHandle Handlers.Logger.Error "Can't editCategory")
      pure $ either (Left . T.pack . displayException) Right tryEdit
    (Right [Just _, Nothing], Just labelParent) -> do
      logMessage logHandle Debug ("Update category: " <> getLabel label)
      logMessage logHandle Debug ("Check parent for category. Parent: " <> getLabel labelParent)
      existLabel <- findCategoryByLabel h labelParent
      case existLabel of
        Left e -> do
          logMessage logHandle Handlers.Logger.Error "function findCategoryByLabel fail"
          pure . Left . T.pack . displayException $ e
        Right Nothing -> do
          logMessage logHandle Warning ("Abort. Parent don't exist: " <> getLabel labelParent)
          pure $ Left "Parent dont' exist"
        Right (Just _) -> do
          logMessage logHandle Debug "Parent exist"
          tryEdit <- editCategory h label newCat
          when (isLeft tryEdit) (logMessage logHandle Handlers.Logger.Error "Can't editCategory")
          pure $ either (Left . T.pack . displayException) Right tryEdit
    _ -> do
      logMessage logHandle Warning ("Abort. Category don't exist or .... Category: " <> getLabel label)
      pure $ Left "Category don't's exist or ..."

createCategoryBase :: (Monad m) => Handle m -> CategoryInternal -> m (Either T.Text Success)
createCategoryBase h cat@(CategoryInternal {..}) = do
  let logHandle = logger h
  logMessage logHandle Debug ("Check category for label for create: " <> getLabel labelCategory)
  exist <- findCategoryByLabel h labelCategory
  case (exist, parentCategory) of
    (Left e, _) -> do
      logMessage logHandle Error "function findCategoryByLabel fail"
      pure . Left . T.pack . displayException $ e
    (Right (Just _), _) -> do
      logMessage logHandle Warning ("Category arleady taken: " <> getLabel labelCategory)
      pure $ Left "Category arleady taken"
    (Right Nothing, Nothing) -> do
      logMessage logHandle Debug ("Create category without parent and label: " <> getLabel labelCategory)
      tryPut <- putCategory h cat
      when (isLeft tryPut) (logMessage logHandle Handlers.Logger.Error "function putCategory fali")
      pure $ either (Left . T.pack . displayException) Right tryPut
    (Right Nothing, Just labelParent) -> do
      logMessage logHandle Debug ("Create category with parent and label: " <> getLabel labelParent <> " " <> getLabel labelCategory)
      logMessage logHandle Debug ("Check parent: " <> getLabel labelParent)
      existLabel <- findCategoryByLabel h labelParent
      case existLabel of
        Left e -> do
          logMessage logHandle Error "function findCategoryByLabel fail"
          pure . Left . T.pack . displayException $ e
        Right Nothing -> do
          logMessage logHandle Warning ("Abort. Parent dont' exist: " <> getLabel labelParent)
          pure $ Left "Parent dont' exist"
        _ -> do
          logMessage logHandle Debug "Parent exist"
          tryPut <- putCategory h cat
          when (isLeft tryPut) (logMessage logHandle Handlers.Logger.Error "function putCategory fail")
          pure $ either (Left . T.pack . displayException) Right tryPut

createUserBase :: (Monad m) => Handle m -> UserInternal -> m (Either T.Text Success)
createUserBase h user@(UserInternal {..}) = do
  let logHandle = logger h
  logMessage logHandle Debug ("check user By login for  create: " <> getLogin loginUser)
  tryFind <- findUserByLogin h loginUser
  case tryFind of
    Left e -> do
      logMessage logHandle Error "function findUserByLogin fail"
      pure . Left . T.pack . displayException $ e
    Right (Just _) -> do
      logMessage logHandle Warning ("Login arleady taken: " <> getLogin loginUser)
      pure $ Left "Login arleady taken"
    Right Nothing -> do
      logMessage logHandle Debug "Create user..."
      time <- getTime h
      --- crypto
      let pwd' = makeHashPassword h passwordUser time -- for make QuasiPassword
      tryCreate <- putUser h (user {passwordUser = MkPasswordUser pwd'}) time
      when (isLeft tryCreate) (logMessage logHandle Handlers.Logger.Error "Can't putUser")
      pure $ either (Left . T.pack . displayException) Right tryCreate
