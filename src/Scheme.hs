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

module Scheme where

import qualified Database.Persist.TH as PTH
import qualified Data.Text as T
import Data.Time (UTCTime)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
 User sql=users
  name T.Text
  login T.Text
  passwordId PasswordId
  created UTCTime
  isAdmin Bool
  isPublisher Bool
  UniqueUser login
  deriving Eq Show
 Password sql=passwords
   quasiPassword T.Text
   deriving Eq Show
 Category sql=categories
  label T.Text
  parent CategoryId Maybe
  UniqueCategory label
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
  UniqueImage header base64
  deriving Eq Show
 ImageBank sql=images_bank -- for tests.
  newsId NewsId
  imageId ImageId
  Primary newsId imageId
  deriving Eq Show
|] 

