{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}

{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
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
import Data.Time (UTCTime, Day)
import Data.Aeson (eitherDecode, encode, ToJSON(..), FromJSON (..))

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
 User sql=users
  name T.Text
  login T.Text
  passwordId PasswordId
  created UTCTime
  isAdmin Bool
  isPublisher Bool
  UniqueUserLogin login
  deriving Eq Show Generic ToJSON
 Password sql=passwords
   quasiPassword T.Text
   deriving Eq Show
 Category sql=categories
  label T.Text
  parent CategoryId Maybe
  UniqueCategoryLabel label
  -- deriving Generic
  deriving Eq Show Generic ToJSON
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
  deriving Eq Show
 ImageBank sql=images_bank -- for tests.
  newsId NewsId
  imageId ImageId
  Primary newsId imageId
  deriving Eq Show
|] 

data FilterItem = FilterDataAt Day | FilterDataUntil Day | FilterDataSince Day
                  | FilterAuthorName  T.Text
                  | FilterCategoryLabel T.Text
                  | FilterTitleFind T.Text
                  | FilterContentFind T.Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
