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
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
--
--
-- for json with Persist
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Base.BasicSchema where
import           Data.Aeson
import           Data.Aeson.Types
import           Database.Persist 

import qualified Database.Persist.TH as PTH
import qualified Database.Persist.Sql as PS
import qualified Data.Text as T
import Data.Time.Calendar
-- import qualified Data.ByteString.Lazy.Char8 as BC
-- import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
-- import GHC.Generics (Generic)
-- import           Database.Persist.Postgresql (

-- import           Database.Persist.Sql

import Data.Tree
import qualified Data.Text.Encoding as E 
import GHC.Generics (Generic)
-- newtype Rose a = Tree a deriving (ToJSON, FromJSON)
-- type Rose = Tree T.Text

-- a :: Rose
-- a = Node ("hi":: T.Text) []
-- data Tree a = Node {
--         rootLabel :: a,         -- ^ label value
--         subForest :: [Tree a]   -- ^ zero or more child trees
--     }
a :: Tree T.Text
a = Node "hi" [Node "a" [], Node "b" []]

newtype Rose = Rose { unRose :: Tree T.Text }
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON)

instance PersistField Rose where
  toPersistValue (Rose x) = (PersistByteString . BC.toStrict . encode) x  -- храним json дерева
  fromPersistValue (PersistByteString t) = maybe (Left "error") (Right . Rose ) $ decode $ BC.fromStrict t

-- instance PersistField Rose where
--   toPersistValue (Rose x) = (PersistText . E.decodeUtf8 . BC.toStrict . encode) x  -- храним json дерева
--   fromPersistValue (PersistText t) =maybe (Left "errorParsePersist") (\x -> Right $ Rose x) $ decode $ BC.fromStrict $ E.encodeUtf8 t
--
instance PS.PersistFieldSql Rose where
  sqlType _ = SqlString
  
-- instance PersistField (Tree T.Text) where
--   toPersistValue = PersistText . E.decodeUtf8 . BC.toStrict . encode  -- храним json дерева
--   fromPersistValue (PersistText t) = helper $ eitherDecode $ BC.fromStrict $ E.encodeUtf8 t  -- :: Either String (Tree T.Text) 
--     where helper :: Either String a -> Either T.Text a
--           helper (Left xs) = Left $ T.pack xs 
--           helper (Right xs) = Right xs
--   fromPersistValue _ = Left "privet"

-- instance PS.PersistFieldSql (Tree T.Text) where
--   sqlType _ = SqlOther " " 

-- printE :: Either T.Text Rose -> IO ()
-- printE (Left x) = print x 
-- printE (Right (Rose x)) = print x
--
dRose :: Either T.Text Rose -> Tree T.Text
dRose (Left x) = Node x []
dRose (Right x) = unRose x

a1 = Rose a
b = toPersistValue a1
c = fromPersistValue b :: Either T.Text Rose

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  News json
    title T.Text
    content T.Text
    data_created Day
-- today <- localDay <$> zonedTimeToLocalTime <$> getZonedTime   :: IO Day
    photos [T.Text]
    publish Bool
    chelId ChelId
    categoryId CategoryId
    UniqueTitle title
    deriving Show Read
  Chel json
    login T.Text
    password T.Text
    data_created T.Text
    admin Bool
    news Bool
    UniqueLogin login
    deriving Show Read
  Category json
    name [T.Text]
    UniqueName name
    deriving Show Read
  CategoryTree json
    tree Rose --T.Text --Rose --T.Text --Rose 
    deriving Show Read
  User json sql=users
    name T.Text
    email T.Text
    age Int
    occupation T.Text
    UniqueEmail email
    deriving Show Read
    -- deriving ToJSON FromJSON
|]

-- aa :: Rose 
-- aa = Node "hi" [] 

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
catTr :: CategoryTree
catTr = CategoryTree { categoryTreeTree = a1 }

cat1 :: Category
cat1 = Category { categoryName = ["c1","c2","c3"] }
cat2 = Category { categoryName = ["c22","c22","c23"] }

chel1 :: Chel
chel1 = Chel {chelLogin = "chel1" , chelPassword = "pass1", chelData_created = "280323", chelAdmin = True, chelNews = True}
chel2 = Chel {chelLogin = "chel2" , chelPassword = "pass2", chelData_created = "280323", chelAdmin = False, chelNews = False}
chel3 = Chel {chelLogin = "chel3" , chelPassword = "pass3", chelData_created = "280323", chelAdmin = False, chelNews = False}

user1:: User
user1 =  User { userName = "User1", userEmail = "User1@test.com" , userAge = 11 , userOccupation = "System Administrator" }
user2=  User { userName = "User2" , userEmail = "User2@test.com" , userAge = 22 , userOccupation = "Byhgalter" }
user3=  User { userName = "User3" , userEmail = "User3@test.com" , userAge = 33 , userOccupation = "Rabotyaga"  }
  
