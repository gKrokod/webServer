{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.Category.EditSpec (spec) where

import Control.Monad.State (State, evalState, gets)
import Data.Binary.Builder as BU (Builder)
import Database.Data.FillTables (cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9)
import Handlers.Database.Base (Success (..))
import qualified Handlers.Database.Base as DB
import qualified Handlers.Database.Category
import qualified Handlers.Logger
import Handlers.Web.Base (CategoryInternal (..))
import Handlers.Web.Category (Handle (..))
import Handlers.Web.Category.Update (updateCategory)
import Network.HTTP.Types (status200)
import Network.Wai (defaultRequest, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (Category (..))
import Test.Hspec (Spec, it, shouldBe)
import Types (Label (..))
import qualified Web.Utils as WU

spec :: Spec
spec = do
  let req = defaultRequest
      req' = req {rawPathInfo = "/categories/edit"}
      categoriesInBase = [cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9]
      logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
            Handlers.Logger.writeLog = \_ -> pure ()
          }
      baseCategoryHandle =
        Handlers.Database.Category.Handle
          { Handlers.Database.Category.logger = logHandle,
            Handlers.Database.Category.userOffset = 0,
            Handlers.Database.Category.userLimit = maxBound,
            Handlers.Database.Category.findCategoryByLabel =
              \(MkLabel label) -> do
                categories <- gets (map categoryLabel)
                pure $
                  Right $
                    if label `elem` categories
                      then Just undefined
                      else Nothing,
            Handlers.Database.Category.putCategory = \_ -> pure $ Right Put,
            Handlers.Database.Category.editCategory = \_label (CategoryInternal _newlabel _parent) -> pure $ Right DB.Change,
            Handlers.Database.Category.pullAllCategories = \_ _ -> pure $ Right []
          }
      categoryHandle =
        Handlers.Web.Category.Handle
          { Handlers.Web.Category.logger = logHandle,
            Handlers.Web.Category.base = baseCategoryHandle,
            Handlers.Web.Category.response400 = WU.response400,
            Handlers.Web.Category.response500 = WU.response500,
            Handlers.Web.Category.response200 = WU.response200,
            Handlers.Web.Category.response404 = WU.response404,
            Handlers.Web.Category.mkGoodResponse = testBuilder,
            Handlers.Web.Category.getBody = \_ -> pure ""
          } ::
          Handlers.Web.Category.Handle (State [Category])

  it "Can edit a category" $ do
    let bodyReq1 = "{\"label\":\"Man\",\"newLabel\":\"NewMan\",\"newparent\":\"Woman\"}"
        bodyReq2 = "{\"label\":\"Man\",\"newLabel\":\"NewMan\"}"
        bodyReq3 = "{\"label\":\"Man\",\"newparent\":\"Woman\"}"
        categoryHandle1 = categoryHandle {Handlers.Web.Category.getBody = const . pure $ bodyReq1}
        categoryHandle2 = categoryHandle {Handlers.Web.Category.getBody = const . pure $ bodyReq2}
        categoryHandle3 = categoryHandle {Handlers.Web.Category.getBody = const . pure $ bodyReq3}

    evalState (updateCategory (error "Proxy AdminRole") categoryHandle1 req') categoriesInBase
      `shouldBe` test200
    evalState (updateCategory (error "Proxy AdminRole") categoryHandle2 req') categoriesInBase
      `shouldBe` test200
    evalState (updateCategory (error "Proxy AdminRole") categoryHandle3 req') categoriesInBase
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
