{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Schema (User (..), Password (..), Category (..), News (..), Image (..), ImageBank (..), IsValidPassword (..), FilterItem (..), ColumnType (..), SortOrder (..), Unique (..), Find (..), EntityField (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Text as T
import Data.Time (Day (..))
import Database.Migrations.Migrationv5 (Category (..), EntityField (..), Image (..), ImageBank (..), News (..), Password (..), Unique (..), User (..))
import GHC.Generics (Generic)

data ColumnType = DataNews | AuthorNews | CategoryName | QuantityImages
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SortOrder = Ascending | Descending
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Find = Find {subString :: T.Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data FilterItem
  = FilterDataAt Day
  | FilterDataUntil Day
  | FilterDataSince Day
  | FilterAuthorName T.Text
  | FilterCategoryLabel T.Text
  | FilterTitleFind T.Text
  | FilterContentFind T.Text
  | FilterPublishOrAuthor (Maybe T.Text)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data IsValidPassword = Valid | NotValid
  deriving (Show)
