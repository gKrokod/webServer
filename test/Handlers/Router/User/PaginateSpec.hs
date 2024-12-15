{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.User.PaginateSpec (spec) where

import Control.Monad.State (State, evalState, get)
import Data.Binary.Builder as BU (Builder)
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
import Web.DTO.User (userToWeb)

spec :: Spec
spec = do
  let req = defaultRequest

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

  it "Client can paginate" $ do
    let req' = req {rawPathInfo = "/users", queryString = [("paginate", Just "{\"offset\":1,\"limit\":1}")]}
        baseHandle' = baseHandle
        client' = WB.Client Nothing Nothing Nothing

        webHandle' =
          webHandle
            { WB.base = baseHandle',
              WB.client = client'
            }

    evalState (doLogic webHandle' req') usersInBase
      `shouldBe` (testBuilder . userToWeb $ take 1 $ drop 1 usersInBase)

testBuilder :: Builder -> Response
testBuilder = responseBuilder status200 []

instance Show Response where
  show (ResponseBuilder s h b) = mconcat [show s, show h, show b]
  show _ = "Response"

instance Eq Response where
  (==) (ResponseBuilder s h b) (ResponseBuilder s' h' b') = (s == s') && (h == h') && (show b == show b')
  (==) _ _ = undefined
