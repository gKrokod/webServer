{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.News.PaginateSpec (spec) where

import Control.Monad.State (State, evalState, get)
import Data.Binary.Builder as BU (Builder)
import Database.Data.FillTables (time4)
import qualified Handlers.Database.Base as DB
import qualified Handlers.Logger
import Handlers.Router (doLogic)
import Handlers.Web.Base (NewsOut (..))
import qualified Handlers.Web.Base as WB
import Network.HTTP.Types (status200)
import Network.Wai (defaultRequest, queryString, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (ColumnType (..), SortOrder (..))
import Test.Hspec (Spec, it, shouldBe)
import Types (Content (..), Label (..), Name (..), Title (..), URI_Image (..))
import Web.DTO.News (newsToWeb)

spec :: Spec
spec = do
  let req = defaultRequest
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

  it "Client can paginate" $ do
    let req' = req {rawPathInfo = "/news", queryString = [("paginate", Just "{\"offset\":1,\"limit\":1}")]}
        baseHandle' = baseHandle
        client' = WB.Client Nothing Nothing Nothing

        webHandle' =
          webHandle
            { WB.base = baseHandle',
              WB.client = client'
            }

    evalState (doLogic webHandle' req') newsInBase
      `shouldBe` (testBuilder . newsToWeb $ take 1 $ drop 1 newsInBase)

testBuilder :: Builder -> Response
testBuilder = responseBuilder status200 []

instance Show Response where
  show (ResponseBuilder s h b) = mconcat [show s, show h, show b]
  show _ = "Response"

instance Eq Response where
  (==) (ResponseBuilder s h b) (ResponseBuilder s' h' b') = (s == s') && (h == h') && (show b == show b')
  (==) _ _ = undefined
