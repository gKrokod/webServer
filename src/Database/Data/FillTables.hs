module Database.Data.FillTables (user1test, user2test, user3test, user1, user2, user3, password1, password2, password3, cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9, imageBank5, imageBank1, imageBank2, imageBank3, imageBank4, news1, news2, news3, news4, image1, image2, image3, time4) where

import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Database.Crypto (makeHashPassword)
import Database.Data.TestImage (testImage1, testImage2, testImage3)
import qualified Database.Migrations.Migrationv0 as SOLD
import Database.Persist.Postgresql (toSqlKey)
import qualified Schema as S
import Types (PasswordUser (..))

time1, time2, time3, time4 :: UTCTime
time1 = UTCTime {utctDay = fromGregorian 2024 10 24, utctDayTime = secondsToDiffTime 1}
time2 = UTCTime {utctDay = fromGregorian 2024 10 24, utctDayTime = secondsToDiffTime 2}
time3 = UTCTime {utctDay = fromGregorian 2024 10 24, utctDayTime = secondsToDiffTime 3}
time4 = UTCTime {utctDay = fromGregorian 2024 10 24, utctDayTime = secondsToDiffTime 4}

user1, user2, user3 :: SOLD.User
user1 =
  SOLD.User
    { SOLD.userName = "user1",
      SOLD.userLogin = "login1",
      SOLD.userPasswordId = toSqlKey 1,
      SOLD.userCreated = time1,
      SOLD.userIsAdmin = True,
      SOLD.userIsPublisher = False
    }
user2 = SOLD.User "user2" "login2" (toSqlKey 2) time2 True True
user3 = SOLD.User "user3" "login3" (toSqlKey 3) time3 False True

password1, password2, password3 :: S.Password
password1 = S.Password {S.passwordQuasiPassword = makeHashPassword (MkPasswordUser "qpass1") time1}
password2 = S.Password (makeHashPassword (MkPasswordUser "qpass2") time2)
password3 = S.Password (makeHashPassword (MkPasswordUser "qpass3") time3)

image1, image2, image3 :: S.Image
image1 = testImage1
image2 = testImage2
image3 = testImage3

cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9 :: S.Category
cat1 = S.Category {S.categoryLabel = "Abstract", S.categoryParent = Nothing}
cat2 = S.Category "Man" (Just $ toSqlKey 1)
cat3 = S.Category "Woman" (Just $ toSqlKey 1)
cat4 = S.Category "Warrior" (Just $ toSqlKey 2)
cat5 = S.Category "Archer" (Just $ toSqlKey 2)
cat6 = S.Category "Neutral" (Just $ toSqlKey 4)
cat7 = S.Category "Evil" (Just $ toSqlKey 4)
cat8 = S.Category "Good" (Just $ toSqlKey 4)
cat9 = S.Category "Witch" (Just $ toSqlKey 3)

news1, news2, news3, news4 :: S.News
news1 =
  S.News
    { S.newsTitle = "News 1 about Witch from user 1",
      S.newsCreated = time1,
      S.newsUserId = toSqlKey 1,
      S.newsCategoryId = toSqlKey 9,
      S.newsContent = "Witch havean apple with photo 1 and 2",
      S.newsIsPublish = False
    }
news2 =
  S.News
    "News 2 about Warriorgfrom user 2"
    time2
    (toSqlKey 2)
    (toSqlKey 4)
    "Warrior like Woman. No photo"
    False
news3 =
  S.News
    "News 3 about Good from user 3"
    time3
    (toSqlKey 3)
    (toSqlKey 8)
    "Good is good. Photo 1 and 3"
    True
news4 =
  S.News
    "News 4 about Evil from user 1"
    time4
    (toSqlKey 1)
    (toSqlKey 7)
    "Evil is evil. Photo 1"
    False

imageBank1, imageBank2, imageBank3, imageBank4, imageBank5 :: S.ImageBank
imageBank1 = S.ImageBank {S.imageBankNewsId = toSqlKey 1, S.imageBankImageId = toSqlKey 1}
imageBank2 = S.ImageBank (toSqlKey 1) (toSqlKey 2)
imageBank3 = S.ImageBank (toSqlKey 3) (toSqlKey 1)
imageBank4 = S.ImageBank (toSqlKey 3) (toSqlKey 3)
imageBank5 = S.ImageBank (toSqlKey 4) (toSqlKey 1)

user1test, user2test, user3test :: S.User
user1test =
  S.User
    { S.userName = "user1",
      S.userLogin = "login1",
      S.userPasswordId = toSqlKey 1,
      S.userCreated = time1,
      S.userIsAdmin = True,
      S.userIsPublisher = False,
      S.userLastName = Just "user1"
    }
user2test = S.User "user2" "login2" (toSqlKey 2) time2 True True (Just "user2")
user3test = S.User "user3" "login3" (toSqlKey 3) time3 False True (Just "user3")
