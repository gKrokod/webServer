{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.News.SortSpec (spec) where

import Control.Monad.State (State, evalState, get)
import Data.Binary.Builder as BU (Builder)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Time (UTCTime (..))
import Database.Data.FillTables (time4, user1test)
import qualified Handlers.Database.Base as DB
import qualified Handlers.Logger
import Handlers.Router (doLogic)
import Handlers.Web.Base (NewsOut (..))
import qualified Handlers.Web.Base as WB
import Network.HTTP.Types (status200)
import Network.Wai (defaultRequest, queryString, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (ColumnType (..), FilterItem (..), Find (..), SortOrder (..), User (..))
import Test.Hspec (Spec, it, shouldBe)
import Types (Content (..), Label (..), Login (..), Name (..), Title (..), URI_Image (..))
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

  it "Client can choouse the type of sorting" $ do
    let req1 = req {rawPathInfo = "/news", queryString = [("sort", Just "{\"columnType\":\"DataNews\",\"sortOrder\":\"Ascending\"}")]}
        req2 = req {rawPathInfo = "/news", queryString = [("sort", Just "{\"columnType\":\"DataNews\",\"sortOrder\":\"Descending\"}")]}
        req3 = req {rawPathInfo = "/news", queryString = [("sort", Just "{\"columnType\":\"AuthorNews\",\"sortOrder\":\"Ascending\"}")]}
        req4 = req {rawPathInfo = "/news", queryString = [("sort", Just "{\"columnType\":\"AuthorNews\",\"sortOrder\":\"Descending\"}")]}
        req5 = req {rawPathInfo = "/news", queryString = [("sort", Just "{\"columnType\":\"CategoryName\",\"sortOrder\":\"Ascending\"}")]}
        req6 = req {rawPathInfo = "/news", queryString = [("sort", Just "{\"columnType\":\"CategoryName\",\"sortOrder\":\"Descending\"}")]}
        req7 = req {rawPathInfo = "/news", queryString = [("sort", Just "{\"columnType\":\"QuantityImages\",\"sortOrder\":\"Ascending\"}")]}
        req8 = req {rawPathInfo = "/news", queryString = [("sort", Just "{\"columnType\":\"QuantityImages\",\"sortOrder\":\"Descending\"}")]}

        baseHandle' =
          baseHandle
            { DB.pullAllNews = \_offset _limit columnType sortOrder find filters ->
                pure . Right $
                  [ MkNewsOut
                      (MkTitle $ typeToText columnType)
                      testTime
                      (MkName $ typeToText sortOrder)
                      []
                      (MkContent $ typeToText find)
                      (map (MkURI_Image . typeToText) filters)
                      False
                  ]
            }

        client1 = WB.Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1test)
        webHandle' =
          webHandle
            { WB.base = baseHandle',
              WB.client = client1
            }

    evalState (doLogic webHandle' req1) newsInBase
      `shouldBe` ( testBuilder . newsToWeb $
                     [ MkNewsOut
                         (MkTitle $ typeToText DataNews)
                         testTime
                         (MkName $ typeToText Ascending)
                         []
                         (MkContent $ typeToText (Nothing :: Maybe Find))
                         [MkURI_Image . typeToText . FilterPublishOrAuthor . Just . userLogin $ user1test]
                         False
                     ]
                 )

    evalState (doLogic webHandle' req2) newsInBase
      `shouldBe` ( testBuilder . newsToWeb $
                     [ MkNewsOut
                         (MkTitle $ typeToText DataNews)
                         testTime
                         (MkName $ typeToText Descending)
                         []
                         (MkContent $ typeToText (Nothing :: Maybe Find))
                         [MkURI_Image . typeToText . FilterPublishOrAuthor . Just . userLogin $ user1test]
                         False
                     ]
                 )
    evalState (doLogic webHandle' req3) newsInBase
      `shouldBe` ( testBuilder . newsToWeb $
                     [ MkNewsOut
                         (MkTitle $ typeToText AuthorNews)
                         testTime
                         (MkName $ typeToText Ascending)
                         []
                         (MkContent $ typeToText (Nothing :: Maybe Find))
                         [MkURI_Image . typeToText . FilterPublishOrAuthor . Just . userLogin $ user1test]
                         False
                     ]
                 )
    evalState (doLogic webHandle' req4) newsInBase
      `shouldBe` ( testBuilder . newsToWeb $
                     -- [(typeToText AuthorNews, testTime, typeToText Descending, [], typeToText (Nothing :: Maybe Find), [typeToText . FilterPublishOrAuthor . Just . userLogin $ user1test], False)])

                     [ MkNewsOut
                         (MkTitle $ typeToText AuthorNews)
                         testTime
                         (MkName $ typeToText Descending)
                         []
                         (MkContent $ typeToText (Nothing :: Maybe Find))
                         [MkURI_Image . typeToText . FilterPublishOrAuthor . Just . userLogin $ user1test]
                         False
                     ]
                 )
    evalState (doLogic webHandle' req5) newsInBase
      `shouldBe` ( testBuilder . newsToWeb $
                     -- [(typeToText CategoryName, testTime, typeToText Ascending, [], typeToText (Nothing :: Maybe Find), [typeToText . FilterPublishOrAuthor . Just . userLogin $ user1test], False)])

                     [ MkNewsOut
                         (MkTitle $ typeToText CategoryName)
                         testTime
                         (MkName $ typeToText Ascending)
                         []
                         (MkContent $ typeToText (Nothing :: Maybe Find))
                         [MkURI_Image . typeToText . FilterPublishOrAuthor . Just . userLogin $ user1test]
                         False
                     ]
                 )
    evalState (doLogic webHandle' req6) newsInBase
      `shouldBe` ( testBuilder . newsToWeb $
                     [ MkNewsOut
                         (MkTitle $ typeToText CategoryName)
                         testTime
                         (MkName $ typeToText Descending)
                         []
                         (MkContent $ typeToText (Nothing :: Maybe Find))
                         [MkURI_Image . typeToText . FilterPublishOrAuthor . Just . userLogin $ user1test]
                         False
                     ]
                 )
    evalState (doLogic webHandle' req7) newsInBase
      `shouldBe` ( testBuilder . newsToWeb $
                     [ MkNewsOut
                         (MkTitle $ typeToText QuantityImages)
                         testTime
                         (MkName $ typeToText Ascending)
                         []
                         (MkContent $ typeToText (Nothing :: Maybe Find))
                         [MkURI_Image . typeToText . FilterPublishOrAuthor . Just . userLogin $ user1test]
                         False
                     ]
                 )
    evalState (doLogic webHandle' req8) newsInBase
      `shouldBe` ( testBuilder . newsToWeb $
                     [ MkNewsOut
                         (MkTitle $ typeToText QuantityImages)
                         testTime
                         (MkName $ typeToText Descending)
                         []
                         (MkContent $ typeToText (Nothing :: Maybe Find))
                         [MkURI_Image . typeToText . FilterPublishOrAuthor . Just . userLogin $ user1test]
                         False
                     ]
                 )

testTime :: UTCTime
testTime = time4

typeToText :: (Show a) => a -> T.Text
typeToText = T.pack . show

testBuilder :: Builder -> Response
testBuilder = responseBuilder status200 []

instance Show Response where
  show (ResponseBuilder s h b) = mconcat [show s, show h, show b]
  show _ = "Response"

instance Eq Response where
  (==) (ResponseBuilder s h b) (ResponseBuilder s' h' b') = (s == s') && (h == h') && (show b == show b')
  (==) _ _ = undefined
