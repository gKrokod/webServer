{-# LANGUAGE TemplateHaskell #-}

module Base.FillTables (user1, user2, user3, password1, password2, password3, cat1,cat2,cat3,cat4,cat5,cat6,cat7,cat8,cat9, imageBank5, imageBank1,imageBank2,imageBank3,imageBank4, news1, news2, news3,news4, image1, image2,image3) where
import qualified Scheme as S
import Base.Crypto (makeHashPassword)
import Base.TestImage (testImage1, testImage2, testImage3)
import Base.LocalTime (localtimeTemplate)
import Database.Persist.Postgresql  (toSqlKey)

user1, user2, user3 :: S.User
user1 =
    S.User { S.userName = "user1"
                 ,S.userLogin = "login1"
                 ,S.userPasswordId = toSqlKey 1
                 ,S.userCreated =  read $(localtimeTemplate)
                 ,S.userIsAdmin = True
                 ,S.userIsPublisher = False }
user2 = S.User "user2" "login2" (toSqlKey 2) (read $(localtimeTemplate)) True True
user3 = S.User "user3" "login3" (toSqlKey 3) (read $(localtimeTemplate)) False True

password1, password2, password3 :: S.Password
password1 = S.Password {S.passwordQuasiPassword = makeHashPassword "qpass1" (read $(localtimeTemplate))}
password2 = S.Password (makeHashPassword "qpass2" (read $(localtimeTemplate)))
password3 = S.Password (makeHashPassword "qpass3" (read $(localtimeTemplate)))

image1, image2, image3 :: S.Image
-- image1  = S.Image { imageHeader = "header1", imageBase64 = "base64 n 1" }
image1  = testImage1 -- real image jpeg
image2  = testImage2 -- real image jpeg
image3  = testImage3 -- real image jpeg
-- image2  = S.Image "image/png" "base64 n 2" 
-- image3  = S.Image "header3" "base64 n 3" 

cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9 :: S.Category
cat1 = S.Category {S.categoryLabel = "Abstract", S.categoryParent = Nothing }
cat2 = S.Category "Man" (Just $ toSqlKey 1)
cat3 = S.Category "Woman" (Just $ toSqlKey 1)
cat4 = S.Category "Warrior" (Just $ toSqlKey 2)
cat5 = S.Category "Archer" (Just $ toSqlKey 2)
cat6 = S.Category "Neutral" (Just $ toSqlKey 4)
cat7 = S.Category "Evil" (Just $ toSqlKey 4)
cat8 = S.Category "Good" (Just $ toSqlKey 4)
cat9 = S.Category "Witch" (Just $ toSqlKey 3)

news1, news2, news3, news4 :: S.News
news1 = S.News { S.newsTitle = "S.News 1 about Witch from user 1",
              S.newsCreated = read $(localtimeTemplate),
              S.newsUserId = toSqlKey 1,
              S.newsCategoryId = toSqlKey 9,
              S.newsContent = "Witch havean apple with photo 1 and 2",
              S.newsIsPublish = False
             }
news2 = S.News "S.News 2 about Warrior from user 2" (read $(localtimeTemplate)) 
             (toSqlKey 2) (toSqlKey 4) "Warrior like Woman. No photo" False
news3 = S.News "S.News 3 about Good from user 3" (read $(localtimeTemplate)) 
             (toSqlKey 3) (toSqlKey 8) "Good is good. Photo 1 and 3" True
news4 = S.News "S.News 4 about Evil from user 1" (read $(localtimeTemplate))
             (toSqlKey 1) (toSqlKey 7) "Evil is evil. Photo 1" False

imageBank1, imageBank2, imageBank3, imageBank4, imageBank5 :: S.ImageBank
imageBank1 = S.ImageBank {S.imageBankNewsId = toSqlKey 1, S.imageBankImageId = toSqlKey 1} 
imageBank2 = S.ImageBank (toSqlKey 1) (toSqlKey 2) 
imageBank3 = S.ImageBank (toSqlKey 3) (toSqlKey 1) 
imageBank4 = S.ImageBank (toSqlKey 3) (toSqlKey 3) 
imageBank5 = S.ImageBank (toSqlKey 4) (toSqlKey 1) 
