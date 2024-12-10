{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Web.DTO (userToWeb, UserFromWeb (..), webToUser, EditNewsFromWeb (..), webToEditNews, newsToWeb, NewsFromWeb (..), webToNews, EditCategoryFromWeb (..), webToEditCategory, categoryToWeb, CategoryFromWeb (..), webToCategory) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.Binary.Builder (Builder, fromLazyByteString)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Handlers.Web.Base (NewsOut (..))
import Schema (Category (..), Image (..), User (..))
import Types (Content (..), Label (..), Name (..), Title (..), URI_Image (..))

data UserToWeb = UserToWeb
  { name :: T.Text,
    login :: T.Text,
    created :: UTCTime,
    isAdmin :: Bool,
    isPublisher :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data UserFromWeb = UserFromWeb
  { name :: T.Text,
    login :: T.Text,
    password :: T.Text,
    isAdmin :: Bool,
    isPublisher :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

userToWeb :: [User] -> Builder
userToWeb = fromLazyByteString . encode @[UserToWeb] . map convertToWeb
  where
    convertToWeb :: User -> UserToWeb
    convertToWeb (User {..}) =
      UserToWeb
        { name = userName,
          login = userLogin,
          created = userCreated,
          isAdmin = userIsAdmin,
          isPublisher = userIsPublisher
        }

webToUser :: B.ByteString -> Either String UserFromWeb
webToUser = eitherDecodeStrict @UserFromWeb

newtype CategoryToWeb = CategoryToWeb {label :: T.Text}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data CategoryFromWeb = CategoryFromWeb {label :: T.Text, parent :: Maybe T.Text}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data EditCategoryFromWeb = EditCategoryFromWeb {label :: T.Text, newlabel :: Maybe T.Text, newparent :: Maybe T.Text}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

categoryToWeb :: [Category] -> Builder
categoryToWeb = fromLazyByteString . encode @[CategoryToWeb] . map convertToWeb
  where
    convertToWeb :: Category -> CategoryToWeb
    convertToWeb c = CategoryToWeb {label = categoryLabel c}

webToCategory :: B.ByteString -> Either String CategoryFromWeb
webToCategory = eitherDecodeStrict @CategoryFromWeb

webToEditCategory :: B.ByteString -> Either String EditCategoryFromWeb
webToEditCategory = eitherDecodeStrict @EditCategoryFromWeb

data NewsFromWeb = NewsFromWeb
  { title :: T.Text,
    login :: T.Text,
    label :: T.Text,
    content :: T.Text,
    images :: [Image],
    isPublish :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

webToNews :: B.ByteString -> Either String NewsFromWeb
webToNews = eitherDecodeStrict @NewsFromWeb

data EditNewsFromWeb = EditNewsFromWeb
  { title :: T.Text,
    newTitle :: Maybe T.Text,
    newLogin :: Maybe T.Text,
    newLabel :: Maybe T.Text,
    newContent :: Maybe T.Text,
    images :: Maybe [Image],
    newIsPublish :: Maybe Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

webToEditNews :: B.ByteString -> Either String EditNewsFromWeb
webToEditNews = eitherDecodeStrict @EditNewsFromWeb

data NewsToWeb = NewsToWeb
  { title :: T.Text,
    created :: UTCTime,
    author :: T.Text,
    labels :: [T.Text],
    content :: T.Text,
    images :: [T.Text],
    isPublish :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

newsToWeb :: [NewsOut] -> Builder
newsToWeb = fromLazyByteString . encode @[NewsToWeb] . map convertToWeb
  where
    convertToWeb :: NewsOut -> NewsToWeb
    convertToWeb newsOut =
      NewsToWeb
        { title = getTitle $ nTitle newsOut,
          created = nTime newsOut,
          author = getName $ nAuthor newsOut,
          labels = map getLabel $ nCategories newsOut,
          content = getContent $ nContent newsOut,
          images = map getURI_Image $ nImages newsOut,
          isPublish = nIsPublish newsOut
        }
