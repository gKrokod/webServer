{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# Language DuplicateRecordFields #-}

module Web.WebType where
import Scheme (User(..), Image(..), Category(..))
import Data.Time (UTCTime)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (eitherDecode, eitherDecodeStrict, encode, ToJSON, FromJSON)
import Data.Binary.Builder (Builder, fromLazyByteString)
import qualified Data.ByteString as B 
  
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

data CategoryToWeb = CategoryToWeb {label :: T.Text}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

categoryToWeb :: [Category] -> Builder
categoryToWeb = fromLazyByteString . encode @[CategoryToWeb] . map convertToWeb
  where convertToWeb :: Category -> CategoryToWeb
        convertToWeb (Category l p) = CategoryToWeb l

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
--
