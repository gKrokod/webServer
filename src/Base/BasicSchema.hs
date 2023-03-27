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
  Person json
    name String
    deriving Show Read 
    -- deriving ToJSON FromJSON
  Store json
    name String
    deriving Show Read
    -- deriving ToJSON FromJSON
  PersonStore json
    personId PersonId
    storeId StoreId
    UniquePersonStore personId storeId
    deriving Show Read
    -- deriving ToJSON FromJSON
  User json sql=users
    name Text
    email Text
    age Int
    occupation Text
    -- test Text
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

user1:: User
user1 =  User
  { userName = "User1"
  , userEmail = "User1@test.com"
  , userAge = 11
  , userOccupation = "System Administrator"
  }

user2:: User
user2=  User
  { userName = "User2"
  , userEmail = "User2@test.com"
  , userAge = 22
  , userOccupation = "Byhgalter"
  }
user3:: User
user3=  User
  { userName = "User3"
  , userEmail = "User3@test.com"
  , userAge = 33
  , userOccupation = "Rabotyaga"
  }
  
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
