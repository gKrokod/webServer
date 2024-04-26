{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
-- for ToJSON and FromJSON
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
-- for json with Persist
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE StandaloneDeriving #-}

module Base.BasicSchema where
import Data.Aeson
import Data.Aeson.Types
import Database.Persist 

import qualified Database.Persist.TH as PTH
import qualified Database.Persist.Sql as PS
import qualified Data.Text as T
import GHC.Generics (Generic)


PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User sql = users
    login T.Text
    name T.Text
    password T.Text
    isAdmin Bool
    isPublisher Bool
    categoryId CategoryId
    UniqueLabel name
    Primary login
    deriving Eq Show
  Category sql = categories
    label T.Text
    UniqueCategory label
    deriving Eq Show
  -- ProductCategory
  --   productId ProductId
  --   categoryId CategoryId
  --   Primary productId categoryId
  --   deriving Eq Show
  -- Warehouse
  --   productId ProductId
  --   quantity Int
  --   -- created UTCTime default=CURRENT_TIME
  --   -- modified UTCTime default=CURRENT_TIME
  --   deriving Eq Show
|]
