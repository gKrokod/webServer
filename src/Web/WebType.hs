{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Web.WebType where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.Binary.Builder (Builder, fromLazyByteString)
import qualified Data.ByteString as B
import Data.ByteString.Base64 as B64
import Data.CaseInsensitive (CI)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Handlers.Web.Web (NewsOut (..))
import Scheme (Category (..), ColumnType (..), FilterItem (..), Find (..), Image (..), SortOrder (..), User (..))
import Types (Content (..), Label (..), Name (..), Title (..), URI_Image (..))

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
  where
    convertToWeb :: Category -> CategoryToWeb
    convertToWeb (Category l _p) = CategoryToWeb l

webToCategory :: B.ByteString -> Either String CategoryFromWeb
webToCategory = eitherDecodeStrict @CategoryFromWeb

webToEditCategory :: B.ByteString -> Either String EditCategoryFromWeb
webToEditCategory = eitherDecodeStrict @EditCategoryFromWeb

data NewsFromWeb = NewsFromWeb
  { title :: T.Text,
    login :: T.Text,
    label :: T.Text,
    content :: T.Text,
    images :: [Image],
    isPublish :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

webToNews :: B.ByteString -> Either String NewsFromWeb
webToNews = eitherDecodeStrict @NewsFromWeb

data EditNewsFromWeb = EditNewsFromWeb
  { title :: T.Text,
    newTitle :: Maybe T.Text,
    newLogin :: Maybe T.Text,
    newLabel :: Maybe T.Text,
    newContent :: Maybe T.Text,
    images :: Maybe [Image],
    newIsPublish :: Maybe Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

webToEditNews :: B.ByteString -> Either String EditNewsFromWeb
webToEditNews = eitherDecodeStrict @EditNewsFromWeb

data NewsToWeb = NewsToWeb
  { title :: T.Text,
    created :: UTCTime,
    author :: T.Text,
    labels :: [T.Text],
    content :: T.Text,
    images :: [T.Text],
    isPublish :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

newsToWeb :: [NewsOut] -> Builder
newsToWeb = fromLazyByteString . encode @[NewsToWeb] . map convertToWeb
  where
    convertToWeb :: NewsOut -> NewsToWeb
    convertToWeb newsOut =
      NewsToWeb
        { title = getTitle $ nTitle newsOut,
          created = nTime newsOut,
          author = getName $ nAuthor newsOut,
          labels = map getLabel $ nCategories newsOut,
          content = getContent $ nContent newsOut,
          images = map getURI_Image $ nImages newsOut,
          isPublish = nIsPublish newsOut
        }

data PanigateFromWeb = Panigate {offset :: Maybe Int, limit :: Maybe Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

webToPanigate :: B.ByteString -> Either String PanigateFromWeb -- from (ByteString, Maybe ByteString)
webToPanigate = eitherDecodeStrict @PanigateFromWeb

type Offset = Int

type Limit = Int

-- queryToPanigate :: Query -> (Int, Int)
-- 0 and maxBound it's default
queryToPanigate :: [(B.ByteString, Maybe B.ByteString)] -> (Offset, Limit)
queryToPanigate = convertFromWeb . mapMaybe (\(x, y) -> if x == "panigate" then y else Nothing)
  where
    convertFromWeb :: [B.ByteString] -> (Int, Int)
    convertFromWeb [xs] = case eitherDecodeStrict @PanigateFromWeb xs of
      Right (Panigate (Just offset') (Just limit')) -> (offset', limit')
      Right (Panigate (Just offset') Nothing) -> (offset', maxBound)
      Right (Panigate Nothing (Just limit')) -> (0, limit')
      _ -> (0, maxBound)
    convertFromWeb _ = (0, maxBound)

data SortFromWeb = SortNews {columnType :: ColumnType, sortOrder :: SortOrder}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- DataNews and Descending it's default
queryToSort :: [(B.ByteString, Maybe B.ByteString)] -> (ColumnType, SortOrder)
queryToSort = convertFromWeb . mapMaybe (\(x, y) -> if x == "sort" then y else Nothing)
  where
    convertFromWeb :: [B.ByteString] -> (ColumnType, SortOrder)
    convertFromWeb [xs] = case eitherDecodeStrict @SortFromWeb xs of
      Right (SortNews column order) -> (column, order)
      _ -> (DataNews, Descending)
    convertFromWeb _ = (DataNews, Descending)

newtype FindFromWeb = FindFromWeb (Maybe Find)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

queryToFind :: [(B.ByteString, Maybe B.ByteString)] -> Maybe Find
queryToFind = convertFromWeb . mapMaybe (\(x, y) -> if x == "find" then y else Nothing)
  where
    convertFromWeb :: [B.ByteString] -> Maybe Find
    convertFromWeb [xs] = case eitherDecodeStrict @FindFromWeb xs of
      Right (FindFromWeb (Just x)) -> Just x
      _ -> Nothing
    convertFromWeb _ = Nothing

newtype FilterFromWeb = FilterFromWeb [FilterItem]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

queryToFilters :: [(B.ByteString, Maybe B.ByteString)] -> [FilterItem]
queryToFilters = convertFromWeb . mapMaybe (\(x, y) -> if x == "filter" then y else Nothing)
  where
    convertFromWeb :: [B.ByteString] -> [FilterItem]
    convertFromWeb [xs] = case eitherDecodeStrict @FilterFromWeb xs of
      Right (FilterFromWeb fs) -> fs
      _ -> []
    convertFromWeb _ = []

-- headersToLoginAndPassword :: [Header]-> (T.Text, T.Text)
-- headersToLoginAndPassword :: [(HeaderName, ByteString)]-> (T.Text, T.Text)
headersToLoginAndPassword :: [(CI B.ByteString, B.ByteString)] -> Maybe (T.Text, T.Text)
headersToLoginAndPassword ((header, loginpass) : xs)
  | header == "Authorization"
      && B.take 6 loginpass == "Basic " =
      Just $ splitLoginPass (B.drop 6 loginpass)
  | otherwise = headersToLoginAndPassword xs
  where
    -- ByteString length >= 6 . proofs early
    splitLoginPass :: B.ByteString -> (T.Text, T.Text)
    splitLoginPass xs' =
      let colon = fromIntegral $ fromEnum ':'
       in case map E.decodeUtf8 . B.splitWith (== colon) . B64.decodeBase64Lenient $ xs' of
            [l, p] -> (l, p)
            _ -> ("", "")
headersToLoginAndPassword [] = Nothing
