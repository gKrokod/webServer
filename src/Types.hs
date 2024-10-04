module Types (NewsEditInternal (..), NewsInternal (..), CategoryInternal (..), UserInternal (..), NewsOut (..), Name (..), Login (..), PasswordUser (..), Label (..), NewLabel, Title (..), Content (..), URI_Image (..), NumberImage (..)) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Scheme (Image (..))

data NewsOut = MkNewsOut
  { nTitle :: Title,
    nTime :: UTCTime,
    nAuthor :: Name,
    nCategories :: [Label],
    nContent :: Content,
    nImages :: [URI_Image],
    nIsPublish :: Bool
  }

newtype Name = MkName {getName :: Text}

newtype Login = MkLogin {getLogin :: Text} deriving (Show, Eq)

newtype PasswordUser = MkPasswordUser {getPasswordUser :: Text} deriving (Show)

newtype Label = MkLabel {getLabel :: Text} deriving (Show, Eq)

type NewLabel = Label

newtype NumberImage = MkNumberImage {getNumberImage :: Int64} deriving (Show)

newtype Title = MkTitle {getTitle :: Text} deriving (Show, Eq)

newtype Content = MkContent {getContent :: Text}

newtype URI_Image = MkURI_Image {getURI_Image :: Text}

data UserInternal = UserInternal
  { nameUser :: Name,
    loginUser :: Login,
    passwordUser :: PasswordUser,
    isAdminUser :: Bool,
    isPublisherUser :: Bool
  }

data CategoryInternal = CategoryInternal
  { labelCategory :: Label,
    parentCategory :: Maybe Label
  }

data NewsInternal = NewsInternal
  { titleNews :: Title,
    authorNews :: Login,
    labelNews :: Label,
    contentNews :: Content,
    imagesNews :: [Image],
    isPublishNews :: Bool
  }

data NewsEditInternal = NewsEditInternal
  { titleEditNews :: Maybe Title,
    authorEditNews :: Maybe Login,
    labelEditNews :: Maybe Label,
    contentEditNews :: Maybe Content,
    imagesEditNews :: [Image],
    isPublishEditNews :: Maybe Bool
  }
