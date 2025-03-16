{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.User.GetSpec (spec) where

import Control.Monad.State (State, evalState, get)
import Data.Binary.Builder as BU (Builder)
import Database.Data.FillTables (time4, user1test, user2test, user3test)
import Handlers.Database.Base (Success (..))
import qualified Handlers.Database.Base as DB
import qualified Handlers.Database.User
import qualified Handlers.Logger
import Handlers.Web.User (Handle (..))
import Handlers.Web.User.Get (existingUsers)
import Network.HTTP.Types (status200)
import Network.Wai (defaultRequest, queryString, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (User (..))
import Test.Hspec (Spec, it, shouldBe)
import Web.DTO.User (userToWeb)
import qualified Web.Utils as WU

spec :: Spec
spec = do
  let req = defaultRequest
      req' = req {rawPathInfo = "/users", queryString = [("paginate", Just "{\"offset\":0,\"limit\":7}")]}
      usersInBase = [user1test, user2test, user3test]
      logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
            Handlers.Logger.writeLog = \_ -> pure ()
          }
      baseUserHandle =
        Handlers.Database.User.Handle
          { Handlers.Database.User.logger = logHandle,
            Handlers.Database.User.userOffset = 0,
            Handlers.Database.User.userLimit = maxBound,
            Handlers.Database.User.getTime = pure time4,
            Handlers.Database.User.makeHashPassword = \_ _ -> "",
            Handlers.Database.User.pullAllUsers = \(DB.MkOffset offset) (DB.MkLimit limit) -> get >>= pure . Right . take limit . drop offset,
            Handlers.Database.User.findUserByLogin = \_ -> pure $ Right Nothing,
            Handlers.Database.User.putUser = \_ _ -> pure $ Right Put
          }
      userHandle =
        Handlers.Web.User.Handle
          { Handlers.Web.User.logger = logHandle,
            Handlers.Web.User.base = baseUserHandle,
            Handlers.Web.User.response400 = WU.response400,
            Handlers.Web.User.response500 = WU.response500,
            Handlers.Web.User.response200 = WU.response200,
            Handlers.Web.User.response404 = WU.response404,
            Handlers.Web.User.mkGoodResponse = testBuilder,
            Handlers.Web.User.getBody = \_ -> pure ""
          } ::
          Handlers.Web.User.Handle (State [User])

  it "Can get a list of users" $ do
    evalState (existingUsers userHandle req') usersInBase
      `shouldBe` (testBuilder . userToWeb $ usersInBase)

  it "Client can paginate" $ do
    let baseUserHandle' = baseUserHandle {Handlers.Database.User.userOffset = 1, Handlers.Database.User.userLimit = 1}
        userHandle' = userHandle {Handlers.Web.User.base = baseUserHandle'}
    evalState (existingUsers userHandle' req') usersInBase
      `shouldBe` (testBuilder . userToWeb $ take 1 $ drop 1 usersInBase)

testBuilder :: Builder -> Response
testBuilder = responseBuilder status200 []

instance Show Response where
  show (ResponseBuilder s h b) = mconcat [show s, show h, show b]
  show _ = "Response"

instance Eq Response where
  (==) (ResponseBuilder s h b) (ResponseBuilder s' h' b') = (s == s') && (h == h') && (show b == show b')
  (==) _ _ = undefined
