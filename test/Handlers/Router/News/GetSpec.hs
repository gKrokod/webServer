{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.News.GetSpec (spec) where

import Control.Monad.State (State, evalState, get)
import Data.Binary.Builder as BU (Builder)
import Data.Proxy (Proxy (..))
import Database.Data.FillTables (time4, user1test, user2test, user3test)
import qualified Handlers.Database.Base as DB
import qualified Handlers.Logger
import Handlers.Router (doLogic)
import Handlers.Web.Base (NewsOut (..))
import qualified Handlers.Web.Base as WB
import Network.HTTP.Types (status200)
import Network.Wai (defaultRequest, queryString, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (ColumnType (..), SortOrder (..), User (..))
import Test.Hspec (Spec, it, shouldBe)
import Types (Content (..), Label (..), Login (..), Name (..), Title (..), URI_Image (..))
import Web.DTO.News (newsToWeb)

spec :: Spec
spec = do
  let req = defaultRequest
      req' = req {rawPathInfo = "/news", queryString = [("paginate", Just "{\"offset\":0,\"limit\":10}")]}
      logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
            Handlers.Logger.writeLog = \_ -> pure ()
          }
      newsInBase =
        [ MkNewsOut
            { nTitle = MkTitle "news1",
              nTime = time4,
              nAuthor = MkName "user1test",
              nCategories = map MkLabel ["Abstract", "Man"],
              nContent = MkContent "content1",
              nImages = [MkURI_Image "/images?id=1"],
              nIsPublish = True
            },
          MkNewsOut
            { nTitle = MkTitle "news2",
              nTime = time4,
              nAuthor = MkName "user2test",
              nCategories = map MkLabel ["Abstract", "Woman"],
              nContent = MkContent "content2",
              nImages = [],
              nIsPublish = True
            },
          MkNewsOut
            { nTitle = MkTitle "news3",
              nTime = time4,
              nAuthor = MkName "user3test",
              nCategories = map MkLabel ["Abstract", "Woman", "Witch"],
              nContent = MkContent "content3",
              nImages = [],
              nIsPublish = False
            },
          MkNewsOut
            { nTitle = MkTitle "news4",
              nTime = time4,
              nAuthor = MkName "user1test",
              nCategories = map MkLabel ["Abstract", "Woman", "Queen"],
              nContent = MkContent "content4",
              nImages = map MkURI_Image ["/images?id=2", "/images?id=3"],
              nIsPublish = True
            }
        ]

      baseHandle =
        DB.Handle
          { DB.logger = logHandle,
            DB.sortColumnNews = DataNews,
            DB.sortOrderNews = Descending,
            DB.findSubString = Nothing,
            DB.filtersNews = [],
            DB.pullAllNews = \(DB.MkOffset offset) (DB.MkLimit limit) _columnType _sortOrder _find _filters -> get >>= pure . Right . take limit . drop offset
          }
      webHandle =
        WB.Handle
          { WB.logger = logHandle,
            WB.base = baseHandle,
            WB.mkGoodResponse = testBuilder
          } ::
          WB.Handle (State [NewsOut])

  it "All clients can get list of news" $ do
    --
    let baseHandle' = baseHandle
        clientAdminUser1 = WB.Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1test)
        clientAdminUser2 = WB.Client (Just Proxy) (Just Proxy) (Just . MkLogin $ userLogin user2test)
        clientAdminUser3 = WB.Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3test)
        clientAdminUser4 = WB.Client Nothing Nothing Nothing

        webHandle1 =
          webHandle
            { WB.base = baseHandle',
              WB.client = clientAdminUser1
            }
        webHandle2 =
          webHandle
            { WB.base = baseHandle',
              WB.client = clientAdminUser2
            }
        webHandle3 =
          webHandle
            { WB.base = baseHandle',
              WB.client = clientAdminUser3
            }
        webHandle4 =
          webHandle
            { WB.base = baseHandle',
              WB.client = clientAdminUser4
            }

    evalState (doLogic webHandle1 req') newsInBase
      `shouldBe` (testBuilder . newsToWeb $ newsInBase)
    evalState (doLogic webHandle2 req') newsInBase
      `shouldBe` (testBuilder . newsToWeb $ newsInBase)
    evalState (doLogic webHandle3 req') newsInBase
      `shouldBe` (testBuilder . newsToWeb $ newsInBase)
    evalState (doLogic webHandle4 req') newsInBase
      `shouldBe` (testBuilder . newsToWeb $ newsInBase)

testBuilder :: Builder -> Response
testBuilder = responseBuilder status200 []

instance Show Response where
  show (ResponseBuilder s h b) = mconcat [show s, show h, show b]
  show _ = "Response"

instance Eq Response where
  (==) (ResponseBuilder s h b) (ResponseBuilder s' h' b') = (s == s') && (h == h') && (show b == show b')
  (==) _ _ = undefined
