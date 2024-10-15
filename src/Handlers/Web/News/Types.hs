module Handlers.Web.News.Types (NewsInternal (..), NewsEditInternal (..), NewsOut (..)) where

import Data.Time (UTCTime)
import Schema (Image (..))
import Types (Content (..), Label (..), Login (..), Name (..), Title (..), URI_Image (..))

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

data NewsOut = MkNewsOut
  { nTitle :: Title,
    nTime :: UTCTime,
    nAuthor :: Name,
    nCategories :: [Label],
    nContent :: Content,
    nImages :: [URI_Image],
    nIsPublish :: Bool
  }
