module Types (Name (..), Login (..), PasswordUser (..), Label (..), NewLabel, Title (..), Content (..), URI_Image (..), NumberImage (..)) where

import Data.Int (Int64)
import Data.Text (Text)

newtype Name = MkName {getName :: Text}

newtype Login = MkLogin {getLogin :: Text} deriving (Show, Eq)

newtype PasswordUser = MkPasswordUser {getPasswordUser :: Text} deriving (Show)

newtype Label = MkLabel {getLabel :: Text} deriving (Show, Eq)

type NewLabel = Label

newtype NumberImage = MkNumberImage {getNumberImage :: Int64} deriving (Show)

newtype Title = MkTitle {getTitle :: Text} deriving (Show, Eq)

newtype Content = MkContent {getContent :: Text}

newtype URI_Image = MkURI_Image {getURI_Image :: Text}
