{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.News.EditSpec (spec) where

import Control.Monad.State (State, evalState, gets)
import Data.Binary.Builder as BU (fromByteString)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Database.Data.FillTables (cat1, news1, news2, news3, news4, user1test, user2test, user3test)
import qualified Handlers.Database.Base as DB
import qualified Handlers.Logger
import Handlers.Router (doLogic)
import Handlers.Web.Base (NewsEditInternal (..))
import qualified Handlers.Web.Base as WB
import Network.HTTP.Types (badRequest400, forbidden403, internalServerError500, notFound404, status200)
import Network.Wai (defaultRequest, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (News (..), User (..))
import Test.Hspec (Spec, it, shouldBe, shouldNotBe)
import Types (Login (..), Title (..))

spec :: Spec
spec = do
  let req = defaultRequest
      req' = req {rawPathInfo = "/news/edit"}

      bodyReq = "{\"title\":\"News 1 about Witch from user 1\",\"newTitle\":\"Edit News1\",\"newIsPublish\":true,\"newLogin\":\"login3\",\"newLabel\":\"Man\",\"newContent\":\"New Content\",\"images\":[{\"imageHeader\":\"edit image\",\"imageBase64\":\"edit kartinka for news sh\"}]}"

      logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
            Handlers.Logger.writeLog = \_ -> pure ()
          }

      newsInBase = [news1, news2, news3, news4]

      baseHandle =
        DB.Handle
          { DB.logger = logHandle,
            DB.findNewsByTitle = \title -> do
              titles <- gets (map (MkTitle . newsTitle))
              pure $
                Right $
                  if title `elem` titles
                    then Just undefined
                    else Nothing,
            DB.findUserByLogin = const (pure $ Right $ Just user1test),
            DB.findCategoryByLabel = const (pure $ Right $ Just cat1),
            DB.editNews = \_title _time (NewsEditInternal _mbtitle _mblogin _mblabel _mbcontent _image _mbp) -> pure $ Right DB.Change
          }
      webHandle =
        WB.Handle
          { WB.logger = logHandle,
            WB.base = baseHandle,
            WB.response200 = test200,
            WB.response404 = test404,
            WB.response403 = test403,
            WB.response400 = test400,
            WB.response500 = test500,
            WB.getBody = const . pure $ bodyReq
          } ::
          WB.Handle (State [News])

  it "Author can edit news" $ do
    let baseHandle' = baseHandle {DB.validCopyRight = \_login _title -> pure $ Right True}

        clientAdminUser1 = WB.Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1test)
        clientAdminUser2 = WB.Client (Just Proxy) (Just Proxy) (Just . MkLogin $ userLogin user2test)
        clientAdminUser3 = WB.Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3test)

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

    evalState (doLogic webHandle1 req') newsInBase
      `shouldBe` test200
    evalState (doLogic webHandle2 req') newsInBase
      `shouldBe` test200
    evalState (doLogic webHandle3 req') newsInBase
      `shouldBe` test200

  it "Non-author can't edit news" $ do
    let baseHandle' = baseHandle {DB.validCopyRight = \_login _title -> pure $ Right False}

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
      `shouldNotBe` test200
    evalState (doLogic webHandle2 req') newsInBase
      `shouldNotBe` test200
    evalState (doLogic webHandle3 req') newsInBase
      `shouldNotBe` test200
    evalState (doLogic webHandle4 req') newsInBase
      `shouldNotBe` test200

test200 :: Response
test200 = responseBuilder status200 [] "All ok. status 200\n"

test400 :: T.Text -> Response
test400 = responseBuilder badRequest400 [] . fromByteString . E.encodeUtf8

test403 :: Response
test403 = responseBuilder forbidden403 [] "Forbidden. status 403\n"

test404 :: Response
test404 = responseBuilder notFound404 [] "NotFound. status 404\n"

test500 :: Response
test500 = responseBuilder internalServerError500 [] "internalServerError. status 500\n"

instance Show Response where
  show (ResponseBuilder s h b) = mconcat [show s, show h, show b]
  show _ = "Response"

instance Eq Response where
  (==) (ResponseBuilder s h b) (ResponseBuilder s' h' b') = (s == s') && (h == h') && (show b == show b')
  (==) _ _ = undefined
