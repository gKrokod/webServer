{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.Category.GetSpec (spec) where

import Control.Monad.State (State, evalState, get)
import Data.Binary.Builder as BU (Builder)
import Database.Data.FillTables (cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9)
import Handlers.Database.Base (Success (..))
import qualified Handlers.Database.Base as DB
import qualified Handlers.Database.Category
import qualified Handlers.Logger
import Handlers.Web.Category (Handle (..))
import Handlers.Web.Category.Get (existingCategories)
import Network.HTTP.Types (status200)
import Network.Wai (defaultRequest, queryString, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (Category (..))
import Test.Hspec (Spec, it, shouldBe)
import Web.DTO.Category (categoryToWeb)
import qualified Web.Utils as WU

spec :: Spec
spec = do
  let req = defaultRequest
      req' = req {rawPathInfo = "/categories", queryString = [("paginate", Just "{\"offset\":0,\"limit\":10}")]}
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
            Handlers.Database.Category.findCategoryByLabel = \_ -> pure $ Right Nothing,
            Handlers.Database.Category.putCategory = \_ -> pure $ Right Put,
            Handlers.Database.Category.editCategory = \_ _ -> pure $ Right Change,
            Handlers.Database.Category.pullAllCategories = \(DB.MkOffset offset) (DB.MkLimit limit) -> get >>= pure . Right . take limit . drop offset
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

  it "Can get a list of category" $ do
    evalState (existingCategories categoryHandle req') categoriesInBase
      `shouldBe` (testBuilder . categoryToWeb $ categoriesInBase)

  it "Client can paginate" $ do
    let baseCategoryHandle' = baseCategoryHandle {Handlers.Database.Category.userOffset = 1, Handlers.Database.Category.userLimit = 1}
        categoryHandle' = categoryHandle {Handlers.Web.Category.base = baseCategoryHandle'}
    evalState (existingCategories categoryHandle' req') categoriesInBase
      `shouldBe` (testBuilder . categoryToWeb $ take 1 $ drop 1 categoriesInBase)

testBuilder :: Builder -> Response
testBuilder = responseBuilder status200 []

instance Show Response where
  show (ResponseBuilder s h b) = mconcat [show s, show h, show b]
  show _ = "Response"

instance Eq Response where
  (==) (ResponseBuilder s h b) (ResponseBuilder s' h' b') = (s == s') && (h == h') && (show b == show b')
  (==) _ _ = undefined
