{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.Category.PaginateSpec (spec) where

import Control.Monad.State (State, evalState, get)
import Data.Binary.Builder as BU (Builder)
import Database.Data.FillTables (cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9)
import qualified Handlers.Database.Base as DB
import qualified Handlers.Logger
import Handlers.Router (doLogic)
import qualified Handlers.Web.Base as WB
import Network.HTTP.Types (status200)
import Network.Wai (defaultRequest, queryString, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (Category (..))
import Test.Hspec (Spec, it, shouldBe)
import Web.DTO.Category (categoryToWeb)

spec :: Spec
spec = do
  it "123" $ do head [23,14] `shouldBe` (23 :: Int)
--   --- todo
--   let req = defaultRequest
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
--   it "Client can paginate" $ do
--     let req' = req {rawPathInfo = "/categories", queryString = [("paginate", Just "{\"offset\":1,\"limit\":1}")]}
--         baseHandle' = baseHandle
--         client' = WB.Client Nothing Nothing Nothing
--
--         webHandle' =
--           webHandle
--             { WB.base = baseHandle',
--               WB.client = client'
--             }
--
--     evalState (doLogic webHandle' req') categoriesInBase
--       `shouldBe` (testBuilder . categoryToWeb $ take 1 $ drop 1 categoriesInBase)
--
-- -- curl "127.0.0.1:4221/images?id=1" --output -
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
