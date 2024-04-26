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
  UuserType json sql=newTable
    naame T.Text
    UniqueNaame naame
    deriving Show Read
    -- deriving ToJSON FromJSON
|]

-- PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
-- PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
--   News json
--     title Title 
--     text_content TextContent
--     data_created Day
--     photo_content [Photo]
--     publish Bool
--     -- chelId ChelId
--     -- categoryId CategoryId
--     UniqueTitle title
--     deriving Show Read
--   User json sql=users
--     login Login 
--     password Password
--     data_created Day 
--     is_admin Bool
--     is_publisher Bool
--     UniqueLogin login
--     deriving Show Read
--   Category json sql=categories
--     name Name 
--     -- categoryDictionaryId CategoryDictionaryId
--     UniqueName name
--     deriving Show Read
--   CategoryDictionary json sql=cat_dictionary
--     tree Rose
--     deriving Show Read
  -- User json sql=users
  --   name Name 
  --   email T.Text
  --   age Int
  --   occupation T.Text
  --   UniqueEmail email
  --   deriving Show Read
    -- deriving ToJSON FromJSON
-- |]

  
-- PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "deleteMigrate"] [PTH.persistLowerCase|
--     -- deriving ToJSON FromJSON
--   User1 json sql=users1
--     name1 Name 
--     email1 T.Text
--     age1 Int
--     occupation1 T.Text
--     deriving Show Read
-- |]

