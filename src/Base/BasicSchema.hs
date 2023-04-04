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
import Data.Time.Calendar
-- import qualified Data.ByteString.Lazy.Char8 as BC
-- import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Data.Tree
-- import qualified Data.Text.Encoding as E 
import GHC.Generics (Generic)

-- data Tree a = Node {
--         rootLabel :: a,         -- ^ label value
--         subForest :: [Tree a]   -- ^ zero or more child trees
--     }

-- for Categore Dictionary
type Rose = Tree T.Text
instance PersistField Rose where
  toPersistValue = PersistByteString . BC.toStrict . encode
  fromPersistValue (PersistByteString t) = maybe (Left "Can't Decode PersistByteString to Tree") Right $ decode $ BC.fromStrict t
  fromPersistValue x = Left $ "\n" <> T.pack (show x)
instance PS.PersistFieldSql Rose where
  sqlType _ = SqlBlob
-- for COntent-TYpe + Image
data Image a = Image { imageHeader :: a, imageContent :: a } 
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON)
type Photo = Image T.Text 

instance PersistField Photo where
  toPersistValue = PersistByteString . BC.toStrict . encode
  fromPersistValue (PersistByteString t) = maybe (Left "Can't Decode PersistByteString to Photo") Right $ decode $ BC.fromStrict t
  fromPersistValue x = Left $ "\n" <> T.pack (show x)
instance PS.PersistFieldSql Photo where
  sqlType _ = SqlBlob

type Login  = T.Text
type Password = T.Text
type Title = T.Text
type TextContent = T.Text
type Name = T.Text

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  News json
    title Title 
    text_content TextContent
    data_created Day
    photo_content [Photo]
    publish Bool
    -- chelId ChelId
    -- categoryId CategoryId
    UniqueTitle title
    deriving Show Read
  Chel json
    login Login 
    password Password
    data_created Day 
    isAdmin Bool
    isPublisher Bool
    UniqueLogin login
    deriving Show Read
  Category json
    name Name 
    -- categoryDictionaryId CategoryDictionaryId
    UniqueName name
    deriving Show Read
  CategoryDictionary json
    tree Rose
    deriving Show Read
  User json sql=users
    name Name 
    email T.Text
    age Int
    occupation T.Text
    UniqueEmail email
    deriving Show Read
    -- deriving ToJSON FromJSON
|]

  
PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "deleteMigrate"] [PTH.persistLowerCase|
    -- deriving ToJSON FromJSON
  User1 json sql=users1
    name1 Name 
    email1 T.Text
    age1 Int
    occupation1 T.Text
    deriving Show Read
|]

