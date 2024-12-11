{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Web.Query (queryToPaginate, queryToFilters, queryToFind, queryToSort, headersToLoginAndPassword) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict)
import qualified Data.ByteString as B
import Data.ByteString.Base64 as B64
import Data.CaseInsensitive (CI)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import GHC.Generics (Generic)
import Schema (ColumnType (..), FilterItem (..), Find (..), SortOrder (..))

data PaginateFromWeb = Paginate {offset :: Maybe Int, limit :: Maybe Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

queryToPaginate :: [(B.ByteString, Maybe B.ByteString)] -> (Int, Int) -- (Offset, Limit)
queryToPaginate = convertFromWeb . mapMaybe (\(x, y) -> if x == "paginate" then y else Nothing)
  where
    convertFromWeb :: [B.ByteString] -> (Int, Int)
    convertFromWeb [xs] = case eitherDecodeStrict @PaginateFromWeb xs of
      Right (Paginate (Just offset') (Just limit')) -> (offset', limit')
      Right (Paginate (Just offset') Nothing) -> (offset', maxBound)
      Right (Paginate Nothing (Just limit')) -> (0, limit')
      _ -> (0, maxBound)
    convertFromWeb _ = (0, maxBound)

data SortFromWeb = SortNews {columnType :: ColumnType, sortOrder :: SortOrder}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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

headersToLoginAndPassword :: [(CI B.ByteString, B.ByteString)] -> Maybe (T.Text, T.Text)
headersToLoginAndPassword ((header, loginpass) : xs)
  | header == "Authorization"
      && B.take 6 loginpass == "Basic " =
      Just $ splitLoginPass (B.drop 6 loginpass)
  | otherwise = headersToLoginAndPassword xs
  where
    splitLoginPass :: B.ByteString -> (T.Text, T.Text)
    splitLoginPass xs' =
      let colon = fromIntegral $ fromEnum ':'
       in case map E.decodeUtf8 . B.splitWith (== colon) . B64.decodeBase64Lenient $ xs' of
            [l, p] -> (l, p)
            _ -> ("", "")
headersToLoginAndPassword [] = Nothing
