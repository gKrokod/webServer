{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

module Scheme where

--

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Text as T
import Data.Time (Day (..), UTCTime (..))
import qualified Database.Persist.TH as PTH
import GHC.Generics (Generic)

PTH.share
  [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"]
  [PTH.persistLowerCase|
 User sql=users
  name T.Text
  login T.Text
  passwordId PasswordId
  created UTCTime
  isAdmin Bool
  isPublisher Bool
  UniqueUserLogin login
  deriving Eq Show
 Password sql=passwords
   quasiPassword T.Text
   deriving Eq Show
 Category sql=categories
  label T.Text
  parent CategoryId Maybe
  UniqueCategoryLabel label
  -- deriving Generic
  deriving Eq Show 
 News sql=news
  title T.Text
  created UTCTime
  userId UserId
  categoryId CategoryId
  content T.Text
  isPublish Bool
  UniqueNews title
  deriving Eq Show
 Image sql=images
  header T.Text
  base64 T.Text
  -- UniqueImage header base64
  deriving Eq Show Generic FromJSON ToJSON
 ImageBank sql=images_bank -- for tests.
  newsId NewsId
  imageId ImageId
  Primary newsId imageId
  deriving Eq Show
|]

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
