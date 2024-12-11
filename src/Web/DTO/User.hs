{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Web.DTO.User (userToWeb, UserFromWeb (..), webToUser) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.Binary.Builder (Builder, fromLazyByteString)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Schema (User (..))

data UserToWeb = UserToWeb
  { name :: T.Text,
    login :: T.Text,
    created :: UTCTime,
    isAdmin :: Bool,
    isPublisher :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data UserFromWeb = UserFromWeb
  { name :: T.Text,
    login :: T.Text,
    password :: T.Text,
    isAdmin :: Bool,
    isPublisher :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

userToWeb :: [User] -> Builder
userToWeb = fromLazyByteString . encode @[UserToWeb] . map convertToWeb
  where
    convertToWeb :: User -> UserToWeb
    convertToWeb (User {..}) =
      UserToWeb
        { name = userName,
          login = userLogin,
          created = userCreated,
          isAdmin = userIsAdmin,
          isPublisher = userIsPublisher
        }

webToUser :: B.ByteString -> Either String UserFromWeb
webToUser = eitherDecodeStrict @UserFromWeb
