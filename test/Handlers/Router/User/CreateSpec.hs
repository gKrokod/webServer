{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.User.CreateSpec (spec) where

import Control.Monad.State (State, evalState, gets)
import Data.Binary.Builder as BU (Builder)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text.Encoding as E
import Database.Data.FillTables (time4, user1test, user2test, user3test)
import qualified Handlers.Logger
import Handlers.Web.Base (UserInternal (..))
import Handlers.Web.User.Create (createUser)
import Network.HTTP.Types (status200)
import Network.Wai (defaultRequest, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (User (..))
import Test.Hspec (Spec, it, shouldBe, shouldNotBe)
import Types (Login (..))
import qualified Handlers.Database.User
import qualified Handlers.Web.User (Handle(..))
import qualified Web.Utils as WU
import Handlers.Database.Base ( Success (..))

spec :: Spec
spec = do
  let req = defaultRequest
      req' = req {rawPathInfo = "/users/create"}
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
            Handlers.Database.User.pullAllUsers = \_ _ -> pure $ Right [],
            Handlers.Database.User.findUserByLogin =
              \(MkLogin login) ->
              gets
                ( Right
                    . listToMaybe
                    . mapMaybe
                      ( \user@(User _ l _ _ _ _ _) ->
                          if l == login then Just user else Nothing
                      )
                ),
            Handlers.Database.User.putUser = \(UserInternal _name _login _pass _admin _publish) _time -> pure $ Right Put
          }
      userHandle = Handlers.Web.User.Handle {
          Handlers.Web.User.logger = logHandle,
          Handlers.Web.User.base = baseUserHandle,
          Handlers.Web.User.response400 = WU.response400,
          Handlers.Web.User.response500 = WU.response500,
          Handlers.Web.User.response200 = WU.response200,
          Handlers.Web.User.response404 = WU.response404,
          Handlers.Web.User.mkGoodResponse = testBuilder,
          Handlers.Web.User.getBody = \_ -> pure "" 
        } :: 
         Handlers.Web.User.Handle (State [User])

  it "Can create new user" $ do
    let bodyReq = "{\"isAdmin\":true,\"isPublisher\":true,\"login\":\"Dager\",\"name\":\"Petr\",\"password\":\"qwerty\"}"
        userHandle' =
          userHandle {Handlers.Web.User.getBody = const . pure $ bodyReq}

    evalState (createUser (error "Admin Role") userHandle' req') usersInBase
      `shouldBe` test200

  it "Can't create a new user with login that already exists in the databse" $ do
    let 
        oldUser = E.encodeUtf8 . userLogin $ user1test
        bodyReq = "{\"isAdmin\":true,\"isPublisher\":true,\"login\":\"" <> oldUser <> "\",\"name\":\"\",\"password\":\"qwerty\"}"
        userHandle' =
          userHandle {Handlers.Web.User.getBody = const . pure $ bodyReq}

    evalState (createUser (error "Admin Role") userHandle' req') usersInBase
      `shouldNotBe` test200

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
