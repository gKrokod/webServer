{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# Language DuplicateRecordFields #-}

module Web.WebType where
import Scheme (User(..), Image(..), Category(..))
-- import Scheme hiding (NewsOut) --(User(..), Image(..), Category(..))
import Data.Time (UTCTime)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (eitherDecode, eitherDecodeStrict, encode, ToJSON, FromJSON)
import Data.Binary.Builder (Builder, fromLazyByteString)
import qualified Data.ByteString as B 
import Data.Maybe
  

data UserToWeb = UserToWeb {name :: T.Text, login :: T.Text, created :: UTCTime, isAdmin :: Bool,
  isPublisher :: Bool}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data UserFromWeb = UserFromWeb {name :: T.Text, login :: T.Text, password :: T.Text, isAdmin :: Bool,
  isPublisher :: Bool}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

userToWeb :: [User] -> Builder
userToWeb = fromLazyByteString . encode @[UserToWeb] . map convertToWeb
  where convertToWeb :: User -> UserToWeb
        convertToWeb (User n l _ c a p) = UserToWeb n l c a p

webToUser :: B.ByteString -> Either String UserFromWeb
webToUser = eitherDecodeStrict @UserFromWeb 

newtype CategoryToWeb = CategoryToWeb {label :: T.Text}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data CategoryFromWeb = CategoryFromWeb {label :: T.Text, parent :: Maybe T.Text}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data EditCategoryFromWeb = EditCategoryFromWeb {label :: T.Text, newlabel :: Maybe T.Text, newparent :: Maybe T.Text}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

categoryToWeb :: [Category] -> Builder
categoryToWeb = fromLazyByteString . encode @[CategoryToWeb] . map convertToWeb
  where convertToWeb :: Category -> CategoryToWeb
        convertToWeb (Category l p) = CategoryToWeb l

webToCategory :: B.ByteString -> Either String CategoryFromWeb 
webToCategory = eitherDecodeStrict @CategoryFromWeb

webToEditCategory :: B.ByteString -> Either String EditCategoryFromWeb 
webToEditCategory = eitherDecodeStrict @EditCategoryFromWeb

data NewsFromWeb = NewsFromWeb {title :: T.Text, login :: T.Text, label :: T.Text, content :: T.Text,
                                images :: [Image], isPublish :: Bool }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

webToNews :: B.ByteString -> Either String NewsFromWeb 
webToNews = eitherDecodeStrict @NewsFromWeb

data EditNewsFromWeb = EditNewsFromWeb {title :: T.Text, newTitle :: Maybe T.Text, 
  newLogin :: Maybe T.Text, newLabel :: Maybe T.Text, 
  newContent :: Maybe T.Text,
  images :: Maybe [Image], newIsPublish :: Maybe Bool }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

webToEditNews :: B.ByteString -> Either String EditNewsFromWeb 
webToEditNews = eitherDecodeStrict @EditNewsFromWeb

data NewsToWeb = NewsToWeb {title :: T.Text, created :: UTCTime, login :: T.Text, 
                            labels :: [T.Text], content :: T.Text, images :: [T.Text], 
                            isPublisher :: Bool}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
--

-- type NewsOut = (Title, UTCTime, Login, [Label], Content, [URI_Image], Bool)
type NewsOut = (T.Text, UTCTime, T.Text, [T.Text], T.Text, [T.Text], Bool)
--
newsToWeb :: [NewsOut] -> Builder
newsToWeb = fromLazyByteString . encode @[NewsToWeb] . map convertToWeb
  where convertToWeb :: NewsOut -> NewsToWeb
        convertToWeb (t, d, l, ls, c, im, b) = NewsToWeb t d l ls c im b 

data PanigateFromWeb = Panigate {offset :: Maybe Int, limit :: Maybe Int }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- webToPanigate :: BQueryItem -> Either String PanigateFromWeb -- from (ByteString, Maybe ByteString) -- ("panigate", Just ...)
webToPanigate :: B.ByteString -> Either String PanigateFromWeb -- from (ByteString, Maybe ByteString)
webToPanigate = eitherDecodeStrict @PanigateFromWeb 

type Offset = Int
type Limit = Int

-- queryToPanigate :: Query -> (Int, Int)
queryToPanigate :: [(B.ByteString, Maybe B.ByteString)] -> (Offset, Limit)
queryToPanigate = convertFromWeb . mapMaybe (\(x,y) -> if x == "panigate" then y else Nothing) 
  where convertFromWeb :: [B.ByteString] -> (Int, Int)
        convertFromWeb [xs] = case (eitherDecodeStrict @PanigateFromWeb xs) of
                                         Right (Panigate (Just offset) (Just limit)) -> (offset, limit)
                                         Right (Panigate (Just offset) Nothing) -> (offset, maxBound)
                                         Right (Panigate Nothing (Just limit)) -> (0, limit)
                                         _ -> (0, maxBound)
        convertFromWeb _ = (0, maxBound)

-- q1 :: [(B.ByteString, Maybe B.ByteString)] -> [B.ByteString]
q1 :: [(B.ByteString, Maybe B.ByteString)] -> [Either String PanigateFromWeb]
q1 = map webToPanigate  . mapMaybe (\(x,y) -> if x == "panigate" then y else Nothing) 
-- type URI_Image = T.Text
--
-- PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
--  User sql=users
--   name T.Text
--   login T.Text
--   passwordId PasswordId
--   created UTCTime
--   isAdmin Bool
--   isPublisher Bool
--   UniqueUserLogin login
--   deriving Eq Show Generic ToJSON
--  Password sql=passwords
--    quasiPassword T.Text
--    deriving Eq Show
--  Category sql=categories
--   label T.Text
--   parent CategoryId Maybe
--   UniqueCategoryLabel label
--   -- deriving Generic
--   deriving Eq Show Generic ToJSON
--  News sql=news
--   title T.Text
--   created UTCTime
--   userId UserId
--   categoryId CategoryId
--   content T.Text
--   isPublish Bool
--   UniqueNews title
--   deriving Eq Show
--  Image sql=images
--   header T.Text
--   base64 T.Text
--   -- UniqueImage header base64
--   deriving Eq Show
--  ImageBank sql=images_bank -- for tests.
--   newsId NewsId
--   imageId ImageId
--   Primary newsId imageId
--   deriving Eq Show
-- |] 
