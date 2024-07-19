{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}

{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Scheme where

import GHC.Generics (Generic)
import qualified Database.Persist.TH as PTH
import qualified Data.Text as T
import Data.Time (UTCTime(..), Day(..))
import Data.Aeson (ToJSON(..), FromJSON (..))
-- import Data.Aeson (eitherDecode, encode, ToJSON(..), FromJSON (..))
-- import Data.Binary.Builder (Builder, fromLazyByteString)
-- import Data.Int (Int64)


-- helper :: Int -> Int
-- helper = succ
-- import Data.Binary.Builder (fromByteString, Builder, fromLazyByteString, putStringUtf8)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
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

data FilterItem = FilterDataAt Day | FilterDataUntil Day | FilterDataSince Day
                  | FilterAuthorName  T.Text
                  | FilterCategoryLabel T.Text
                  | FilterTitleFind T.Text
                  | FilterContentFind T.Text
                  | FilterPublishOrAuthor (Maybe T.Text)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- data Privilege = Anonymous | WebUser | Admin
--   deriving stock (Eq, Show, Generic)
--   -- deriving anyclass (ToJSON, FromJSON)

data IsValidPassword = Valid | NotValid
  deriving Show


-- type FilterFromWeb = [FilterItem]
-- encode (FilterDataAt (a <- utctDay <$> getCurrentTime))       "{\"contents\":\"2024-07-14\",\"tag\":\"FilterDataAt\"}
-- ghci> decode @FilterItem c
-- Just (FilterDataAt 2024-07-14)

-- data Filter = Filter { name :: FilterItem, value :: T.Text}

-- type Name = T.Text
-- type Login = T.Text
-- type Time = UTCTime
-- type PasswordUser = T.Text
-- type Label = T.Text
-- type NewLabel = T.Text
-- type NumberImage = Int64
-- type Header = T.Text
-- type Base64 = T.Text
-- type Title = T.Text
-- type Content = T.Text
-- type URI_Image = T.Text
-- type NewsOut = (Title, UTCTime, Login, [Label], Content, [URI_Image], Bool)
