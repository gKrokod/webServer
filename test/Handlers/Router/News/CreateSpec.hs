{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.News.CreateSpec (spec) where

import Control.Monad.Identity (Identity, runIdentity)
import Data.Binary.Builder as BU (fromByteString)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Database.Data.FillTables (cat1, time4, user1test, user2test)
import qualified Handlers.Database.Base as DB
import qualified Handlers.Logger
import Handlers.Router (doLogic)
import Handlers.Web.Base (NewsInternal (..))
import qualified Handlers.Web.Base as WB
import Network.HTTP.Types (badRequest400, forbidden403, internalServerError500, notFound404, status200)
import Network.Wai (defaultRequest, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (User (..))
import Test.Hspec (Spec, it, shouldBe, shouldNotBe)
import Types (Login (..))

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

      baseHandle =
        DB.Handle
          { DB.logger = logHandle,
            DB.getTime = pure time4,
            DB.findNewsByTitle = const (pure $ Right Nothing),
            DB.findUserByLogin = const (pure $ Right $ Just user1test),
            DB.findCategoryByLabel = const (pure . Right $ Just cat1),
            DB.putNews = \(NewsInternal _title _login _label _content _images _isPublish) _time -> pure $ Right DB.Put
          }
      webHandle =
        WB.Handle
          { WB.logger = logHandle,
            WB.base = baseHandle,
            WB.response404 = test404,
            WB.response403 = test403,
            WB.response400 = test400,
            WB.response500 = test500,
            WB.response200 = test200,
            WB.getBody = const . pure $ bodyReq
          } ::
          WB.Handle Identity

  it "Publisher can create news" $ do
    let baseHandle' = baseHandle
        clientAdminUser2 = WB.Client (Just Proxy) (Just Proxy) (Just . MkLogin $ userLogin user2test)
        webHandle' =
          webHandle
            { WB.base = baseHandle',
              WB.client = clientAdminUser2
            }
    runIdentity (doLogic webHandle' req')
      `shouldBe` test200

  it "Non-publisher can't create news" $ do
    let baseHandle' = baseHandle
        clientAdminUser1 = WB.Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1test)
        webHandle' =
          webHandle
            { WB.base = baseHandle',
              WB.client = clientAdminUser1
            }

    runIdentity (doLogic webHandle' req')
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
