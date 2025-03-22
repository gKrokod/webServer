{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Config (ConfigDataBase (..), loadConfig, connectionString, whenFillTestData, ServerSetup, makeSetup) where

import Control.Exception (SomeException, displayException, throwIO, try)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as E (encodeUtf8)
import Data.Time (getCurrentTime)
import qualified Database.Api as DA
import Database.Persist.Postgresql (ConnectionString)
import GHC.Generics (Generic)
import qualified Handlers.Database.Auth
import qualified Handlers.Database.Category
import qualified Handlers.Database.Image
import qualified Handlers.Database.News
import qualified Handlers.Database.User
import Handlers.Logger
import qualified Handlers.Web.Base
import qualified Handlers.Web.Category
import qualified Handlers.Web.Image
import qualified Handlers.Web.News
import qualified Handlers.Web.User
import qualified Logger
import Schema (ColumnType (..), SortOrder (..))
import qualified Web.Utils as WU

type ServerSetup m = Handlers.Web.Base.Handle m

data ConfigDataBase = MkConfigDataBase
  { cHostDB :: T.Text,
    cPortDB :: T.Text,
    cUserDB :: T.Text,
    cNameDB :: T.Text,
    cPasswordDB :: T.Text,
    cLimitData :: Int,
    cPortServer :: Int,
    cLogLvl :: Log,
    cFillTestData :: DoItOrSkip
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DoItOrSkip = DoIt | Skip
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

whenFillTestData :: (Applicative f) => ConfigDataBase -> f () -> f ()
whenFillTestData cfg action
  | cFillTestData cfg == DoIt = action
  | otherwise = pure ()

loadConfigDB :: IO (Either String ConfigDataBase)
loadConfigDB =
  either (Left . displayException) eitherDecode
    <$> try @SomeException (L.readFile "config/db.cfg")

loadConfig :: IO ConfigDataBase
loadConfig = do
  cfg <- loadConfigDB
  case cfg of
    Left error' -> throwIO $ userError error'
    Right config -> pure config

connectionString :: ConfigDataBase -> ConnectionString
connectionString cfg =
  E.encodeUtf8 $
    mconcat
      [ "host=",
        cHostDB cfg,
        " port=",
        cPortDB cfg,
        " user=",
        cUserDB cfg,
        " dbname=",
        cNameDB cfg,
        " password=",
        cPasswordDB cfg
      ]

makeSetup :: ConfigDataBase -> IO (ServerSetup IO)
makeSetup cfg = do
  let pginfo = connectionString cfg
  let logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = cLogLvl cfg,
            Handlers.Logger.writeLog = Logger.writeLog
          }
      client =
        Handlers.Database.Auth.Client
          { Handlers.Database.Auth.clientAdminToken = Nothing,
            Handlers.Database.Auth.clientPublisherToken = Nothing,
            Handlers.Database.Auth.author = Nothing
          }
      authHandle =
        Handlers.Database.Auth.Handle
          { Handlers.Database.Auth.logger = logHandle,
            Handlers.Database.Auth.findUserByLogin = DA.findUserByLogin pginfo,
            Handlers.Database.Auth.validPassword = DA.validPassword pginfo,
            Handlers.Database.Auth.client = client,
            Handlers.Database.Auth.validCopyRight = DA.validCopyRight pginfo
          }
      baseImageHandle =
        Handlers.Database.Image.Handle
          { Handlers.Database.Image.logger = logHandle,
            Handlers.Database.Image.pullImage = DA.pullImage pginfo
          }
      imageHandle =
        Handlers.Web.Image.Handle
          { Handlers.Web.Image.logger = logHandle,
            Handlers.Web.Image.base = baseImageHandle
          }
      baseCategoryHandle =
        Handlers.Database.Category.Handle
          { Handlers.Database.Category.logger = logHandle,
            Handlers.Database.Category.userOffset = 0,
            Handlers.Database.Category.userLimit = maxBound,
            Handlers.Database.Category.findCategoryByLabel = DA.findCategoryByLabel pginfo,
            Handlers.Database.Category.putCategory = DA.putCategory pginfo,
            Handlers.Database.Category.editCategory = DA.editCategory pginfo,
            Handlers.Database.Category.pullAllCategories = DA.pullAllCategories pginfo (cLimitData cfg)
          }
      categoryHandle =
        Handlers.Web.Category.Handle
          { Handlers.Web.Category.logger = logHandle,
            Handlers.Web.Category.base = baseCategoryHandle,
            Handlers.Web.Category.getBody = WU.getBody
          }
      baseUserHandle =
        Handlers.Database.User.Handle
          { Handlers.Database.User.logger = logHandle,
            Handlers.Database.User.userOffset = 0,
            Handlers.Database.User.userLimit = maxBound,
            Handlers.Database.User.getTime = getCurrentTime,
            Handlers.Database.User.makeHashPassword = DA.makeHashPassword,
            Handlers.Database.User.pullAllUsers = DA.pullAllUsers pginfo (cLimitData cfg),
            Handlers.Database.User.findUserByLogin = DA.findUserByLogin pginfo,
            Handlers.Database.User.putUser = DA.putUser pginfo
          }
      userHandle =
        Handlers.Web.User.Handle
          { Handlers.Web.User.logger = logHandle,
            Handlers.Web.User.base = baseUserHandle,
            Handlers.Web.User.getBody = WU.getBody
          }
      baseNewsHandle =
        Handlers.Database.News.Handle
          { Handlers.Database.News.logger = logHandle,
            Handlers.Database.News.userOffset = 0,
            Handlers.Database.News.userLimit = maxBound,
            Handlers.Database.News.getTime = getCurrentTime,
            Handlers.Database.News.findUserByLogin = DA.findUserByLogin pginfo,
            Handlers.Database.News.sortColumnNews = DataNews,
            Handlers.Database.News.sortOrderNews = Descending,
            Handlers.Database.News.findSubString = Nothing,
            Handlers.Database.News.filtersNews = [],
            Handlers.Database.News.findCategoryByLabel = DA.findCategoryByLabel pginfo,
            Handlers.Database.News.putNews = DA.putNews pginfo,
            Handlers.Database.News.findNewsByTitle = DA.findNewsByTitle pginfo,
            Handlers.Database.News.pullAllNews = DA.pullAllNews pginfo (cLimitData cfg),
            Handlers.Database.News.editNews = DA.editNews pginfo
          }
      newsHandle =
        Handlers.Web.News.Handle
          { Handlers.Web.News.logger = logHandle,
            Handlers.Web.News.base = baseNewsHandle,
            Handlers.Web.News.auth = authHandle,
            Handlers.Web.News.client = client,
            Handlers.Web.News.getBody = WU.getBody
          }
      handle =
        Handlers.Web.Base.Handle
          { Handlers.Web.Base.connectionString = pginfo,
            Handlers.Web.Base.logger = logHandle,
            Handlers.Web.Base.auth = authHandle,
            Handlers.Web.Base.client = client,
            Handlers.Web.Base.user = userHandle,
            Handlers.Web.Base.category = categoryHandle,
            Handlers.Web.Base.news = newsHandle,
            Handlers.Web.Base.image = imageHandle
          }
  pure handle
