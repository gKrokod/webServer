{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.Category.EditSpec (spec) where

import Control.Monad.State (State, evalState, gets)
import Data.Binary.Builder as BU (fromByteString)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Database.Data.FillTables (cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9, time4, user1test, user3test)
import qualified Handlers.Database.Base as DB
import qualified Handlers.Logger
import Handlers.Router (doLogic)
import Handlers.Web.Base (CategoryInternal (..))
import qualified Handlers.Web.Base as WB
import Network.HTTP.Types (badRequest400, forbidden403, internalServerError500, notFound404, status200)
import Network.Wai (defaultRequest, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (Category (..), User (..))
import Test.Hspec (Spec, it, shouldBe, shouldNotBe)
import Types (Label (..), Login (..))

spec :: Spec
spec = do
  let req = defaultRequest
      req' = req {rawPathInfo = "/categories/edit"}
      logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
            Handlers.Logger.writeLog = \_ -> pure ()
          }

      categoriesInBase = [cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9]

      baseHandle =
        DB.Handle
          { DB.logger = logHandle,
            DB.getTime = pure time4,
            DB.findCategoryByLabel = \(MkLabel label) -> do
              categories <- gets (map categoryLabel)
              pure $
                Right $
                  if label `elem` categories
                    then Just undefined
                    else Nothing,
            DB.editCategory = \_label (CategoryInternal _newlabel _parent) -> pure $ Right DB.Change
          }
      webHandle =
        WB.Handle
          { WB.logger = logHandle,
            WB.base = baseHandle,
            WB.response404 = test404,
            WB.response403 = test403,
            WB.response400 = test400,
            WB.response500 = test500,
            WB.response200 = test200
          } ::
          WB.Handle (State [Category])

  it "Admin can edit a category" $ do
    let bodyReq1 = "{\"label\":\"Man\",\"newLabel\":\"NewMan\",\"newparent\":\"Woman\"}"
        bodyReq2 = "{\"label\":\"Man\",\"newLabel\":\"NewMan\"}"
        bodyReq3 = "{\"label\":\"Man\",\"newparent\":\"Woman\"}"
        baseHandle' = baseHandle
        clientAdminUser1 = WB.Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1test)
        webHandle1 =
          webHandle
            { WB.base = baseHandle',
              WB.client = clientAdminUser1,
              WB.getBody = const . pure $ bodyReq1
            }
        webHandle2 =
          webHandle
            { WB.base = baseHandle',
              WB.client = clientAdminUser1,
              WB.getBody = const . pure $ bodyReq2
            }
        webHandle3 =
          webHandle
            { WB.base = baseHandle',
              WB.client = clientAdminUser1,
              WB.getBody = const . pure $ bodyReq3
            }

    evalState (doLogic webHandle1 req') categoriesInBase
      `shouldBe` test200
    evalState (doLogic webHandle2 req') categoriesInBase
      `shouldBe` test200
    evalState (doLogic webHandle3 req') categoriesInBase
      `shouldBe` test200

  it "Non-admin can't edit a category" $ do
    let bodyReq = "{\"label\":\"Man\",\"newLabel\":\"NewMan\",\"newparent\":\"Woman\"}"
        baseHandle' = baseHandle
        clientAdminUser3 = WB.Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3test)
        webHandle' =
          webHandle
            { WB.base = baseHandle',
              WB.client = clientAdminUser3,
              WB.getBody = const . pure $ bodyReq
            }
    evalState (doLogic webHandle' req') categoriesInBase
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
