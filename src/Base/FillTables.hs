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
image2  = Image "header2" "base64 n 2" 
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
