{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.Category.GetSpec (spec) where

import Control.Monad.State (State, evalState, get)
import Data.Binary.Builder as BU (Builder)
import Data.Proxy (Proxy (..))
import Database.Data.FillTables (cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9, user1test, user2test, user3test)
import qualified Handlers.Database.Base as DB
import qualified Handlers.Logger
import Handlers.Router (doLogic)
import qualified Handlers.Web.Base as WB
import Network.HTTP.Types (status200)
import Network.Wai (defaultRequest, queryString, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (Category (..), User (..))
import Test.Hspec (Spec, it, shouldBe)
import Types (Login (..))
import Web.DTO.Category (categoryToWeb)

spec :: Spec
spec = do
  it "123" $ do head [23,14] `shouldBe` (23 :: Int)
--   --- todo
--   let req = defaultRequest
--       req' = req {rawPathInfo = "/categories", queryString = [("paginate", Just "{\"offset\":0,\"limit\":10}")]}
--       logHandle =
--         Handlers.Logger.Handle
--           { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
--             Handlers.Logger.writeLog = \_ -> pure ()
--           }
--
--       categoriesInBase = [cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9]
--
--       baseHandle =
--         DB.Handle
--           { DB.logger = logHandle,
--             DB.pullAllCategories = \(DB.MkOffset offset) (DB.MkLimit limit) -> get >>= pure . Right . take limit . drop offset
--           }
--       webHandle =
--         WB.Handle
--           { WB.logger = logHandle,
--             WB.base = baseHandle,
--             WB.mkGoodResponse = testBuilder
--           } ::
--           WB.Handle (State [Category])
--
--   it "All clients can get a list of category" $ do
--     let baseHandle' = baseHandle
--         clientAdminUser1 = WB.Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1test)
--         clientAdminUser2 = WB.Client (Just Proxy) (Just Proxy) (Just . MkLogin $ userLogin user2test)
--         clientAdminUser3 = WB.Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3test)
--         clientAdminUser4 = WB.Client Nothing Nothing Nothing
--
--         webHandle1 =
--           webHandle
--             { WB.base = baseHandle',
--               WB.client = clientAdminUser1
--             }
--         webHandle2 =
--           webHandle
--             { WB.base = baseHandle',
--               WB.client = clientAdminUser2
--             }
--         webHandle3 =
--           webHandle
--             { WB.base = baseHandle',
--               WB.client = clientAdminUser3
--             }
--         webHandle4 =
--           webHandle
--             { WB.base = baseHandle',
--               WB.client = clientAdminUser4
--             }
--
--     evalState (doLogic webHandle1 req') categoriesInBase
--       `shouldBe` (testBuilder . categoryToWeb $ categoriesInBase)
--     evalState (doLogic webHandle2 req') categoriesInBase
--       `shouldBe` (testBuilder . categoryToWeb $ categoriesInBase)
--     evalState (doLogic webHandle3 req') categoriesInBase
--       `shouldBe` (testBuilder . categoryToWeb $ categoriesInBase)
--     evalState (doLogic webHandle4 req') categoriesInBase
--       `shouldBe` (testBuilder . categoryToWeb $ categoriesInBase)
--
-- testBuilder :: Builder -> Response
-- testBuilder = responseBuilder status200 []
--
-- instance Show Response where
--   show (ResponseBuilder s h b) = mconcat [show s, show h, show b]
--   show _ = "Response"
--
-- instance Eq Response where
--   (==) (ResponseBuilder s h b) (ResponseBuilder s' h' b') = (s == s') && (h == h') && (show b == show b')
--   (==) _ _ = undefined
