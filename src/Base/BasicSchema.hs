-- {-# LANGUAGE TemplateHaskell            #-}
-- {-# LANGUAGE QuasiQuotes                #-}
-- {-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE MultiParamTypeClasses      #-}
-- {-# LANGUAGE GADTs                      #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE RecordWildCards            #-}
-- {-# LANGUAGE FlexibleInstances          #-}
-- {-# LANGUAGE OverloadedStrings          #-}
-- {-# LANGUAGE DerivingStrategies         #-}
-- {-# LANGUAGE StandaloneDeriving         #-}
-- {-# LANGUAGE UndecidableInstances       #-}
--
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
--
-- for ToJSON and FromJSON
-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE DeriveAnyClass #-}
--
--
-- for json with Persist
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Base.BasicSchema where
import           Data.Aeson
import           Data.Aeson.Types
import           Database.Persist (Entity(..), Entity)
import qualified Database.Persist.TH as PTH
import           Data.Text (Text)
-- import GHC.Generics (Generic)
-- import           Database.Persist.Postgresql (

-- import           Database.Persist.Sql

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  News json
    short_title Text
    full_title Text
    data_created Text
    photos [Text]
    publish Bool
    chelId ChelId
    categoryId CategoryId
    UniqueFTitle full_title
    UniqueSTitle short_title
    deriving Show Read
  Chel json
    login Text
    password Text
    data_created Text
    admin Bool
    news Bool
    UniqueLogin login
    deriving Show Read
  Category json
    language Text
    flowers Text
    deriving Show Read
  -- Person json
  --   name String
  --   deriving Show Read 
  --   -- deriving ToJSON FromJSON
  User json sql=users
    name Text
    email Text
    age Int
    occupation Text
    UniqueEmail email
    deriving Show Read
    -- deriving ToJSON FromJSON
|]

-- sampleUser :: Entity User
-- sampleUser = Entity (toSqlKey 1) $ User
--   { userName = "admin"
--   , userEmail = "admin@test.com"
--   , userAge = 23
--   , userOccupation = "System Administrator"
--   }


data Item = N (News) | U (User) | C (Chel) | Ca (Category)

-- news1 :: News
-- news1 = News { newsShort_title = "shortN1", newsFull_title = "fullN1", newsData_created = "151515", newsPhotos = ["photo1", "photo2"], newsPublish = True }--P, newsChelId = undefined :: ChelId, newsCategoryId = undefined }

cat1 :: Category
cat1 = Category { categoryLanguage = "Haskell", categoryFlowers = "rose"}

chel1 :: Chel
chel1 = Chel {chelLogin = "chel1" , chelPassword = "pass1", chelData_created = "280323", chelAdmin = True, chelNews = True}
chel2 = Chel {chelLogin = "chel2" , chelPassword = "pass2", chelData_created = "280323", chelAdmin = False, chelNews = False}
chel3 = Chel {chelLogin = "chel3" , chelPassword = "pass3", chelData_created = "280323", chelAdmin = False, chelNews = False}

user1:: User
user1 =  User { userName = "User1", userEmail = "User1@test.com" , userAge = 11 , userOccupation = "System Administrator" }
user2=  User { userName = "User2" , userEmail = "User2@test.com" , userAge = 22 , userOccupation = "Byhgalter" }
user3=  User { userName = "User3" , userEmail = "User3@test.com" , userAge = 33 , userOccupation = "Rabotyaga"  }
  
-- instance ToJSON User where
--   toJSON user = object 
--     [ "name" .= userName user
--     , "email" .= userEmail user
--     , "age" .= userAge user
--     , "occupation" .= userOccupation user
--     ]
--
-- instance FromJSON User where
--   parseJSON = withObject "User" parseUser
--
-- parseUser :: Object -> Parser User
-- parseUser o = do
--   uName <- o .: "name"
--   uEmail <- o .: "email"
--   uAge <- o .: "age"
--   uOccupation <- o .: "occupation"
--   return User
--     { userName = uName
--     , userEmail = uEmail
--     , userAge = uAge
--     , userOccupation = uOccupation
--     }
