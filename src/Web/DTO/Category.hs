{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Web.DTO.Category (EditCategoryFromWeb (..), webToEditCategory, categoryToWeb, CategoryFromWeb (..), webToCategory, EditCategoryIdFromWeb (..), webToEditIdCategory) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.Binary.Builder (Builder, fromLazyByteString)
import qualified Data.ByteString as B
import Data.Int (Int64)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Schema (Category (..))

newtype CategoryToWeb = CategoryToWeb {label :: T.Text}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data CategoryFromWeb = CategoryFromWeb {label :: T.Text, parent :: Maybe T.Text}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data EditCategoryFromWeb = EditCategoryFromWeb {label :: T.Text, newlabel :: Maybe T.Text, newparent :: Maybe T.Text}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data EditCategoryIdFromWeb = EditCategoryIdFromWeb {uid :: Int64, newlabel :: Maybe T.Text, newparent :: Maybe T.Text}
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

webToEditIdCategory :: B.ByteString -> Either String EditCategoryIdFromWeb
webToEditIdCategory = eitherDecodeStrict @EditCategoryIdFromWeb
