module Base.TestEntity where

import qualified Data.Text as T
import Data.Tree
import Base.BasicSchema
import Database.Persist 
-- import Data.Time.Calendar
import Data.Time

data Item = N (News) | U (User) | C (User) | Ca (Category)

-- today <- localDay <$> zonedTimeToLocalTime <$> getZonedTime   :: IO Day
-- news1 :: IO (News)
news1, news2 :: News
news1 =
  -- today <- localDay <$> zonedTimeToLocalTime <$> getZonedTime --  :: IO Day
  News { newsTitle = "News Title", newsText_content = "Text Content1", newsData_created = fromGregorian 2023 3 31, newsPhoto_content = [Image "Content-Type: image/png" "photo1", Image "Content-Type: image/jpeg" "photo2"], newsPublish = True }

news2 = News { newsTitle = "News Title2", newsText_content = "Text Content", newsData_created = fromGregorian 2023 3 31, newsPhoto_content = [Image "Content-Type: image/png" "photo1"], newsPublish = True }

catTr1 :: CategoryDictionary
catTr1 = CategoryDictionary { categoryDictionaryTree = Node "Abstract" [Node "Man" [Node "Warrior" [Node "Evil" [], Node "Good" [], Node "Neutral" []], Node "Archer" []], Node "Woman" [Node "Witch" []]] }

cat1, cat2 :: Category
cat1 = Category { categoryName = "Man" }
cat2 = Category { categoryName = "Woman" }
cat3 = Category { categoryName = "Abstract" }
cat4 = Category { categoryName = "Warrior" }
cat5 = Category { categoryName = "Archer" }
cat6 = Category { categoryName = "Witch" }
cat7 = Category { categoryName = "Evil" }
cat8 = Category { categoryName = "Good" }
cat9 = Category { categoryName = "Neutral" }

user1,user2,user3,user4,user5,user6 :: User
user1 = User {userLogin = "user1" , userPassword = "pass1", userData_created = fromGregorian 2000 5 7, userIs_admin = True,  userIs_publisher = True}
user2 = User {userLogin = "user2" , userPassword = "pass2", userData_created = fromGregorian 1995 7 12, userIs_admin = False, userIs_publisher = False}
user3 = User {userLogin = "user3" , userPassword = "pass3", userData_created = fromGregorian 1987 1 1, userIs_admin = False, userIs_publisher = False}

user4 = User {userLogin = "user4" , userPassword = "pass1", userData_created = fromGregorian 1500 2 15, userIs_admin = True,  userIs_publisher = True}
user5 = User {userLogin = "user5" , userPassword = "pass2", userData_created = fromGregorian 1600 3 19, userIs_admin = False, userIs_publisher = False}
user6 = User {userLogin = "user6" , userPassword = "pass3", userData_created = fromGregorian 1800 11 11, userIs_admin = False, userIs_publisher = False}
-- user1:: User
-- user1 =  User { userName = "User1", userEmail = "User1@test.com" , userAge = 11 , userOccupation = "System Administrator" }
-- user2=  User { userName = "User2" , userEmail = "User2@test.com" , userAge = 22 , userOccupation = "Byhgalter" }
-- user3=  User { userName = "User3" , userEmail = "User3@test.com" , userAge = 33 , userOccupation = "Rabotyaga"  }
--
testTree :: Rose
testTree =  Node "Abstract" [Node "Man" [Node "Warrior" [Node "Evil" [], Node "Good" [], Node "Neutral" []], Node "Archer" []], Node "Woman" [Node "Witch" []]] 

-- -- tt :: Tree String
-- tt =  Node (Just "Abstract") [Node (Just "Man") [Node (Just "Warrior") [Node (Just "Evil") [], Node (Just "Good") [], Node (Just "Neutral") []], Node (Just "Archer") []], Node (Just "Woman") [Node (Just "Witch") []]] 
-- t2, t3 :: Tree (Maybe T.Text)
-- t2 =  Node (Just "Abstract") [Node (Just "Man") [Node (Just "Warrior") [Node (Just "Evil") [], Node (Just "Good") [], Node (Just "Neutral") []], Node (Just "Archer") []], Node (Nothing) [Node (Just "Witch") []]] 
-- t3 =  Node (Just "Abstract") [Node (Nothing ) [Node (Just "Warrior") [Node (Just "Evil") [], Node (Just "Good") [], Node (Just "Neutral") []], Node (Just "Archer") []], Node (Just "woman") [Node (Just "Witch") []]] 
-- -- type Rose = Tree T.Text
--
