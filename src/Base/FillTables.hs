{-# LANGUAGE TemplateHaskell            #-}

module Base.FillTables where
import Scheme
import Data.Time --(UTCTime)
import Base.LocalTime
import Data.Int (Int64)
import Database.Persist.Postgresql  (toSqlKey)

user1,user2,user3 :: User
user1 =
    User { userName = "user1"
                 , userLogin = "login1"
                 , userPasswordId = (toSqlKey 1)
                 , userCreated = read $(localtimeTemplate)
                 , userIsAdmin = True
                 , userIsPublisher = False }

user2 = User { userName = "user2"
             , userLogin = "login2"
             , userPasswordId = (toSqlKey 2)
             , userCreated = read  $(localtimeTemplate)
             , userIsAdmin = True
             , userIsPublisher = True }

user3  = User { userName = "user3"
             , userLogin = "login3"
             , userPasswordId = (toSqlKey 3)
             , userCreated = read  $(localtimeTemplate)
             , userIsAdmin = False
             , userIsPublisher = True }

password1, password2, password3 :: Password
password1 = Password "qpass1"
password2 = Password "qpass2"
password3 = Password "qpass3"

image1, image2, image3 :: Image
image1  = Image "header1" "base64 n 1" 
image2  = Image "header2" "base64 n 2" 
image3  = Image "header3" "base64 n 3" 


cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9 :: Category
cat1 = Category "Abstact" Nothing 
cat2 = Category "Man" (Just $ toSqlKey 1)
cat3 = Category "Woman" (Just $ toSqlKey 1)
cat4 = Category "Warrior" (Just $ toSqlKey 2)
cat5 = Category "Archer" (Just $ toSqlKey 2)
cat6 = Category "Neutral" (Just $ toSqlKey 4)
cat7 = Category "Evil" (Just $ toSqlKey 4)
cat8 = Category "Good" (Just $ toSqlKey 4)
cat9 = Category "Witch" (Just $ toSqlKey 3)

