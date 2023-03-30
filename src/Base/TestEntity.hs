module Base.TestEntity where

import qualified Data.Text as T
import Data.Tree
import Base.BasicSchema
import           Database.Persist 
-- import Data.Time.Calendar
import Data.Time
-- aa :: Rose 
-- aa = Node "hi" [] 

-- sampleUser :: Entity User
-- sampleUser = Entity (toSqlKey 1) $ User
--   { userName = "admin"
--   , userEmail = "admin@test.com"
--   , userAge = 23
--   , userOccupation = "System Administrator"
--   }

a :: Tree T.Text
a = Node "Abstract" [Node "Man" [Node "Warrior" [Node "Evil" [], Node "Good" [], Node "Neutral" []], Node "Archer" []], Node "Woman" [Node "Witch" []]]
-- a1 = Rose a
-- b = toPersistValue a1
-- c = fromPersistValue b :: Either T.Text Rose

data Item = N (News) | U (User) | C (Chel) | Ca (Category)

-- today <- localDay <$> zonedTimeToLocalTime <$> getZonedTime   :: IO Day
news1 :: IO (News)
news1 = do
  today <- localDay <$> zonedTimeToLocalTime <$> getZonedTime --  :: IO Day
  pure $ News { newsTitle = "News Title", 
    newsText_content = "Text Content", newsData_created = today,
    newsPhoto_content = [Image "Content-Type: image/png" "photo1", Image "Content-Type: image/jpeg" "photo2"],
    newsPublish = True } --P, newsChelId = undefined :: ChelId, newsCategoryId = undefined }

catTr1 :: CategoryDictionary
catTr1 = CategoryDictionary { categoryDictionaryTree = a }
--
-- for Rose
-- catTr :: CategoryTree
-- catTr = CategoryTree { categoryTreeTree = a1 }

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
{--
newtype Rose = Rose { unRose :: Tree T.Text }
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON)

instance PersistField Rose where
  toPersistValue (Rose x) = (PersistByteString . BC.toStrict . encode) x  -- храним json дерева
  -- toPersistValue (Rose x) = (PersistText . E.decodeUtf8 . BC.toStrict . encode) x  -- храним json дерева
  fromPersistValue (PersistByteString t) = maybe (Left "error") (Right . Rose ) $ decode $ BC.fromStrict t
  -- fromPersistValue (PersistText t) =maybe (Left "che taktoe errorParsePersist") (\x -> Right $ Rose x) $ decode $ BC.fromStrict $ E.encodeUtf8 t
  fromPersistValue x = Left $ "\n" <> T.pack (show x)
--
-- instance PersistField Rose where
--   toPersistValue (Rose x) = (PersistText . E.decodeUtf8 . BC.toStrict . encode) x  -- храним json дерева
--   fromPersistValue (PersistText t) =maybe (Left "errorParsePersist") (\x -> Right $ Rose x) $ decode $ BC.fromStrict $ E.encodeUtf8 t
--
instance PS.PersistFieldSql Rose where
  -- sqlType _ = SqlString
  sqlType _ = SqlBlob
--
-- printE :: Either T.Text Rose -> IO ()
-- printE (Left x) = print x 
-- printE (Right (Rose x)) = print x
--
-- dRose :: Either T.Text Rose -> Tree T.Text
-- dRose (Left x) = Node x []
-- dRose (Right x) = unRose x
  --}
