{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Web.DTO.News (EditNewsFromWeb (..), webToEditNews, newsToWeb, NewsFromWeb (..), webToNews, newsWithIdToWeb) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.Binary.Builder (Builder, fromLazyByteString)
import qualified Data.ByteString as B
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Handlers.Web.Base (NewsOut (..), NewsOutWithId (..))
import Schema (Image (..))
import Types (Content (..), Label (..), LabelAndId (..), Name (..), Title (..), URI_Image (..))

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

data NewsWithIdToWeb = NewsWithIdToWeb
  { title :: T.Text,
    id :: Int64,
    created :: UTCTime,
    author :: T.Text,
    labels :: [T.Text],
    content :: T.Text,
    images :: [T.Text],
    isPublish :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

newsWithIdToWeb :: [NewsOutWithId] -> Builder
newsWithIdToWeb = fromLazyByteString . encode @[NewsWithIdToWeb] . map convertToWeb
  where
    convertToWeb :: NewsOutWithId -> NewsWithIdToWeb
    convertToWeb newsOut =
      NewsWithIdToWeb
        { title = getTitle $ wTitle newsOut,
          id = wId newsOut,
          created = wTime newsOut,
          author = getName $ wAuthor newsOut,
          labels = map getLabelAndId $ wCategories newsOut,
          content = getContent $ wContent newsOut,
          images = map getURI_Image $ wImages newsOut,
          isPublish = wIsPublish newsOut
        }
