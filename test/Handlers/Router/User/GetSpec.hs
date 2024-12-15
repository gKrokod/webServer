{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.User.GetSpec (spec) where

import Control.Monad.State (State, evalState, get)
import Data.Binary.Builder as BU (Builder)
import Data.Proxy (Proxy (..))
import Database.Data.FillTables (user1test, user2test, user3test)
import qualified Handlers.Database.Base as DB
import qualified Handlers.Logger
import Handlers.Router (doLogic)
import qualified Handlers.Web.Base as WB
import Network.HTTP.Types (status200)
import Network.Wai (defaultRequest, queryString, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (User (..))
import Test.Hspec (Spec, it, shouldBe)
import Types (Login (..))
import Web.DTO.User (userToWeb)

spec :: Spec
spec = do
  let req = defaultRequest
      req' = req {rawPathInfo = "/users", queryString = [("paginate", Just "{\"offset\":0,\"limit\":7}")]}

      logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
            Handlers.Logger.writeLog = \_ -> pure ()
          }

      usersInBase = [user1test, user2test, user3test]
      baseHandle =
        DB.Handle
          { DB.logger = logHandle,
            DB.pullAllUsers = \(DB.MkOffset offset) (DB.MkLimit limit) -> get >>= pure . Right . take limit . drop offset
          }
      webHandle =
        WB.Handle
          { WB.logger = logHandle,
            WB.base = baseHandle,
            WB.mkGoodResponse = testBuilder
          } ::
          WB.Handle (State [User])

  it "All clients can get a list of users" $ do
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

    evalState (doLogic webHandle1 req') usersInBase
      `shouldBe` (testBuilder . userToWeb $ usersInBase)
    evalState (doLogic webHandle2 req') usersInBase
      `shouldBe` (testBuilder . userToWeb $ usersInBase)
    evalState (doLogic webHandle3 req') usersInBase
      `shouldBe` (testBuilder . userToWeb $ usersInBase)
    evalState (doLogic webHandle4 req') usersInBase
      `shouldBe` (testBuilder . userToWeb $ usersInBase)

testBuilder :: Builder -> Response
testBuilder = responseBuilder status200 []

instance Show Response where
  show (ResponseBuilder s h b) = mconcat [show s, show h, show b]
  show _ = "Response"

instance Eq Response where
  (==) (ResponseBuilder s h b) (ResponseBuilder s' h' b') = (s == s') && (h == h') && (show b == show b')
  (==) _ _ = undefined
