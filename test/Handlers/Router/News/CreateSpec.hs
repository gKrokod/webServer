{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.News.CreateSpec (spec) where

import Control.Monad.Identity (Identity, runIdentity)
import Data.Binary.Builder as BU (Builder)
import Database.Data.FillTables (cat1, time4, user1test)
import qualified Handlers.Database.Auth
import Handlers.Database.Base (Success (..))
import qualified Handlers.Database.News
import qualified Handlers.Logger
import Handlers.Web.Base (NewsInternal (..))
import Handlers.Web.News (Handle (..))
import Handlers.Web.News.Create (createNews)
import Network.HTTP.Types (status200)
import Network.Wai (defaultRequest, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (ColumnType (..), SortOrder (..))
import Test.Hspec (Spec, it, shouldBe)
import qualified Web.Utils as WU

spec :: Spec
spec = do
  let req = defaultRequest
      req' = req {rawPathInfo = "/news/create"}
      bodyReq = "{\"title\":\"News from SH script\",\"isPublish\":false,\"login\":\"login1\",\"label\":\"Witch\",\"content\":\"New text about news from sh\",\"images\":[{\"imageHeader\":\"image\",\"imageBase64\":\"kartinka for news sh\"},{\"imageHeader\":\"image2 sh\",\"imageBase64\":\"kartinka for news sh\"}]}"

      logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
            Handlers.Logger.writeLog = \_ -> pure ()
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
            Handlers.Database.Auth.findUserByLogin = \_ -> pure $ Right Nothing,
            Handlers.Database.Auth.validPassword = \_ _ -> pure $ Right True,
            Handlers.Database.Auth.client = client,
            Handlers.Database.Auth.validCopyRight = \_ _ -> pure $ Right True
          }
      baseNewsHandle =
        Handlers.Database.News.Handle
          { Handlers.Database.News.logger = logHandle,
            Handlers.Database.News.userOffset = 0,
            Handlers.Database.News.userLimit = maxBound,
            Handlers.Database.News.getTime = pure time4,
            Handlers.Database.News.findUserByLogin = const (pure $ Right $ Just user1test),
            Handlers.Database.News.sortColumnNews = DataNews,
            Handlers.Database.News.sortOrderNews = Descending,
            Handlers.Database.News.findSubString = Nothing,
            Handlers.Database.News.filtersNews = [],
            Handlers.Database.News.findCategoryByLabel = const (pure . Right $ Just cat1),
            Handlers.Database.News.putNews = \(NewsInternal _title _login _label _content _images _isPublish) _time -> pure $ Right Put,
            Handlers.Database.News.findNewsByTitle = const (pure $ Right Nothing),
            Handlers.Database.News.pullAllNews = \_ _ _columntype _sortorder _find _filters -> pure $ Right [],
            Handlers.Database.News.editNews = \_ _ _ -> pure $ Right Change
          }

      newsHandle =
        Handlers.Web.News.Handle
          { Handlers.Web.News.logger = logHandle,
            Handlers.Web.News.base = baseNewsHandle,
            Handlers.Web.News.auth = authHandle,
            Handlers.Web.News.client = client,
            Handlers.Web.News.response400 = WU.response400,
            Handlers.Web.News.response500 = WU.response500,
            Handlers.Web.News.response200 = WU.response200,
            Handlers.Web.News.response404 = WU.response404,
            Handlers.Web.News.response403 = WU.response403,
            Handlers.Web.News.mkGoodResponse = testBuilder,
            Handlers.Web.News.getBody = const . pure $ bodyReq
          } ::
          Handlers.Web.News.Handle Identity

  it "Can create news" $ do
    runIdentity (createNews (error "Publisher Role") newsHandle req')
      `shouldBe` test200

test200 :: Response
test200 = responseBuilder status200 [] "All ok. status 200\n"

instance Show Response where
  show (ResponseBuilder s h b) = mconcat [show s, show h, show b]
  show _ = "Response"

instance Eq Response where
  (==) (ResponseBuilder s h b) (ResponseBuilder s' h' b') = (s == s') && (h == h') && (show b == show b')
  (==) _ _ = undefined

testBuilder :: Builder -> Response
testBuilder = responseBuilder status200 []
