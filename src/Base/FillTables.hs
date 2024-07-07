{-# LANGUAGE TemplateHaskell            #-}

module Base.FillTables where
  -- data for test database
import Scheme
import Data.Time (UTCTime)
import Base.LocalTime (localtimeTemplate)
-- import Data.Int (Int64)
import Database.Persist.Postgresql  (toSqlKey)

user1,user2,user3 :: User
user1 =
    User { userName = "user1"
                 , userLogin = "login1"
                 , userPasswordId = (toSqlKey 1)
                 , userCreated =  read $(localtimeTemplate)
                 , userIsAdmin = True
                 , userIsPublisher = False }
user2 = User "user2" "login2" (toSqlKey 2) (read $(localtimeTemplate)) True True
user3 = User "user3" "login3" (toSqlKey 3) (read $(localtimeTemplate)) False True

password1, password2, password3 :: Password
password1 = Password { passwordQuasiPassword = "qpass1"}
password2 = Password "qpass2"
password3 = Password "qpass3"

image1, image2, image3 :: Image
image1  = Image { imageHeader = "header1", imageBase64 = "base64 n 1" }
-- image1  = testImage
image2  = Image "image/png" "base64 n 2" 
image3  = Image "header3" "base64 n 3" 

cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9 :: Category
cat1 = Category {categoryLabel = "Abstract", categoryParent = Nothing }
cat2 = Category "Man" (Just $ toSqlKey 1)
cat3 = Category "Woman" (Just $ toSqlKey 1)
cat4 = Category "Warrior" (Just $ toSqlKey 2)
cat5 = Category "Archer" (Just $ toSqlKey 2)
cat6 = Category "Neutral" (Just $ toSqlKey 4)
cat7 = Category "Evil" (Just $ toSqlKey 4)
cat8 = Category "Good" (Just $ toSqlKey 4)
cat9 = Category "Witch" (Just $ toSqlKey 3)

news1, news2, news3, news4 :: News
news1 = News { newsTitle = "News 1 about Witch from user 1",
               newsCreated = read $(localtimeTemplate),
               newsUserId = toSqlKey 1,
               newsCategoryId = toSqlKey 9,
               newsContent = "Witch havean apple with photo 1 and 2",
               newsIsPublish = False
             }
news2 = News "News 2 about Warrior from user 2" (read $(localtimeTemplate)) 
             (toSqlKey 2) (toSqlKey 4) "Warrior like Woman. No photo" False
news3 = News "News 3 about Good from user 3" (read $(localtimeTemplate)) 
             (toSqlKey 3) (toSqlKey 8) "Good is good. Photo 1 and 3" True
news4 = News "News 4 about Evil from user 1" (read $(localtimeTemplate))
             (toSqlKey 1) (toSqlKey 7) "Evil is evil. Photo 1" False

imageBank1, imageBank2, imageBank3, imageBank4, imageBank5 :: ImageBank
imageBank1 = ImageBank {imageBankNewsId = (toSqlKey 1), imageBankImageId = (toSqlKey 1)} 
imageBank2 = ImageBank (toSqlKey 1) (toSqlKey 2) 
imageBank3 = ImageBank (toSqlKey 3) (toSqlKey 1) 
imageBank4 = ImageBank (toSqlKey 3) (toSqlKey 3) 
imageBank5 = ImageBank (toSqlKey 4) (toSqlKey 1) 


testImage = Image "image/png" "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"
