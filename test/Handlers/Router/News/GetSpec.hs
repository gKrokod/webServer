{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.News.GetSpec (spec) where

import Control.Monad.State (State, evalState, get)
import Data.Binary.Builder as BU (Builder)
import Database.Data.FillTables (time4)
import qualified Handlers.Database.Base as DB
import qualified Handlers.Logger
import Handlers.Web.Base (NewsOut (..))
import Handlers.Web.News.Get (existingNews)
import Network.HTTP.Types (status200)
import Network.Wai (defaultRequest, queryString, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (ColumnType (..), SortOrder (..))
import Test.Hspec (Spec, it, shouldBe)
import Types (Content (..), Label (..), Name (..), Title (..), URI_Image (..))
import Web.DTO.News (newsToWeb)
import Handlers.Web.News (Handle (..))
import qualified Handlers.Database.Auth
import qualified Handlers.Database.News
import qualified Web.Utils as WU
import Handlers.Database.Base ( Success (..))

spec :: Spec
spec = do
  let req = defaultRequest
      req' = req {rawPathInfo = "/news", queryString = [("paginate", Just "{\"offset\":0,\"limit\":10}")]}
      newsInBase =
        [ MkNewsOut
            { nTitle = MkTitle "news1",
              nTime = time4,
              nAuthor = MkName "user1test",
              nCategories = map MkLabel ["abstract", "man"],
              nContent = MkContent "content1",
              nImages = [MkURI_Image "/images?id=1"],
              nIsPublish = True
            },
          MkNewsOut
            { nTitle = MkTitle "news2",
              nTime = time4,
              nAuthor = MkName "user2test",
              nCategories = map MkLabel ["abstract", "woman"],
              nContent = MkContent "content2",
              nImages = [],
              nIsPublish = True
            },
          MkNewsOut
            { nTitle = MkTitle "news3",
              nTime = time4,
              nAuthor = MkName "user3test",
              nCategories = map MkLabel ["abstract", "woman", "witch"],
              nContent = MkContent "content3",
              nImages = [],
              nIsPublish = False
            },
          MkNewsOut
            { nTitle = MkTitle "news4",
              nTime = time4,
              nAuthor = MkName "user1test",
              nCategories = map MkLabel ["abstract", "woman", "queen"],
              nContent = MkContent "content4",
              nImages = map MkURI_Image ["/images?id=2", "/images?id=3"],
              nIsPublish = True
            }
        ]
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
            Handlers.Database.News.findUserByLogin = \_ -> pure $ Right Nothing,
            Handlers.Database.News.sortColumnNews = DataNews,
            Handlers.Database.News.sortOrderNews = Descending,
            Handlers.Database.News.findSubString = Nothing,
            Handlers.Database.News.filtersNews = [],
            Handlers.Database.News.findCategoryByLabel = \_ -> pure $ Right Nothing,
            Handlers.Database.News.putNews = \_ _ -> pure $ Right Put,
            Handlers.Database.News.findNewsByTitle = \_ -> pure $ Right Nothing,
            Handlers.Database.News.pullAllNews = \(DB.MkOffset offset) (DB.MkLimit limit) _columntype _sortorder _find _filters -> get >>= pure . Right . take limit . drop offset,
            Handlers.Database.News.editNews = \_ _ _ -> pure $ Right Change
          }

      newsHandle = Handlers.Web.News.Handle {
          Handlers.Web.News.logger = logHandle,
          Handlers.Web.News.base = baseNewsHandle,
          Handlers.Web.News.auth = authHandle,
          Handlers.Web.News.client = client,
          Handlers.Web.News.response400 = WU.response400,
          Handlers.Web.News.response500 = WU.response500,
          Handlers.Web.News.response200 = WU.response200,
          Handlers.Web.News.response404 = WU.response404,
          Handlers.Web.News.response403 = WU.response403,
          Handlers.Web.News.mkGoodResponse = testBuilder,
          Handlers.Web.News.getBody = \_ -> pure ""
        } ::
         Handlers.Web.News.Handle (State [NewsOut])

  it "Clients can get list of news" $ do
    evalState (existingNews newsHandle req')  newsInBase
      `shouldBe` (testBuilder . newsToWeb $ newsInBase)

  it "Client can paginate" $ do
    let
      baseNewsHandle' = baseNewsHandle {Handlers.Database.News.userOffset = 1, Handlers.Database.News.userLimit = 1}
      newsHandle' = newsHandle {Handlers.Web.News.base = baseNewsHandle'}

    evalState (existingNews newsHandle' req')  newsInBase
      `shouldBe` (testBuilder . newsToWeb $ take 1 $ drop 1 newsInBase)

testBuilder :: Builder -> Response
testBuilder = responseBuilder status200 []

instance Show Response where
  show (ResponseBuilder s h b) = mconcat [show s, show h, show b]
  show _ = "Response"

instance Eq Response where
  (==) (ResponseBuilder s h b) (ResponseBuilder s' h' b') = (s == s') && (h == h') && (show b == show b')
  (==) _ _ = undefined
