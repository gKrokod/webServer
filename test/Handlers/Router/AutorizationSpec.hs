{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.AutorizationSpec (spec) where

import Control.Monad.State (State, evalState, gets)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Database.Data.FillTables (user1test, user2test, user3test)
import Handlers.Database.Auth (Client (..))
import qualified Handlers.Database.Auth
import qualified Handlers.Database.Auth as Auth
import qualified Handlers.Logger
import Handlers.Router (doAuthorization)
import qualified Handlers.Web.Base
import Network.Wai (defaultRequest, requestHeaders)
import Network.Wai.Internal (Response (..))
import Schema (User (..))
import Test.Hspec (Spec, it, shouldBe, shouldNotBe)
import Types (Login (..))

spec :: Spec
spec = do
  let req = defaultRequest
      usersInBase = [user1test, user2test, user3test]
      logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
            Handlers.Logger.writeLog = \_ -> pure ()
          }
      authHandle =
        Handlers.Database.Auth.Handle
          { Handlers.Database.Auth.logger = logHandle,
            Handlers.Database.Auth.findUserByLogin =
              \(MkLogin login) ->
                gets
                  ( Right
                      . listToMaybe
                      . mapMaybe
                        ( \user@(User _ l _ _ _ _ _) ->
                            if l == login then Just user else Nothing
                        )
                  ),
            Handlers.Database.Auth.validPassword = \_login _password -> pure $ Right True,
            Handlers.Database.Auth.client = Client Nothing Nothing Nothing,
            Handlers.Database.Auth.validCopyRight = \_ _ -> pure $ Right True
          }
      handle =
        Handlers.Web.Base.Handle
          { Handlers.Web.Base.logger = logHandle,
            Handlers.Web.Base.auth = authHandle,
            Handlers.Web.Base.connectionString = undefined,
            Handlers.Web.Base.user = undefined,
            Handlers.Web.Base.category = undefined,
            Handlers.Web.Base.image = undefined,
            Handlers.Web.Base.news = undefined,
            Handlers.Web.Base.client = undefined
          } ::
          Handlers.Web.Base.Handle (State [User])

  it "Unknown user does not received privileges" $ do
    let req' = req {requestHeaders = []}
        handle' = handle

    Handlers.Web.Base.client <$> evalState (doAuthorization handle' req') usersInBase
      `shouldBe` Right (Client Nothing Nothing Nothing)

  it "A user with a valid password receives their privileges" $ do
    let req1 = req {requestHeaders = [("Authorization", "Basic bG9naW4xOnFwYXNzMQ==")]} -- user1test
        req2 = req {requestHeaders = [("Authorization", "Basic bG9naW4yOnFwYXNzMg==")]} -- user2test
        req3 = req {requestHeaders = [("Authorization", "Basic bG9naW4zOnFwYXNzMw==")]} -- user3test
        handle' = handle

    Handlers.Web.Base.client <$> evalState (doAuthorization handle' req1) usersInBase
      `shouldBe` Right (Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1test)) -- user1test
    Handlers.Web.Base.client <$> evalState (doAuthorization handle' req2) usersInBase
      `shouldBe` Right (Client (Just Proxy) (Just Proxy) (Just . MkLogin $ userLogin user2test)) -- user2test
    Handlers.Web.Base.client <$> evalState (doAuthorization handle' req3) usersInBase
      `shouldBe` Right (Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3test)) -- user3test
  it "A user with an invalid password does not receive his privileges" $ do
    let req1 = req {requestHeaders = [("Authorization", "Basic bG9naW4xOk5PQ09SUkVDVFBBU1NXT1JE")]} -- user1test
        req2 = req {requestHeaders = [("Authorization", "Basic bG9naW4yOk5PQ09SUkVDVFBBU1NXT1JE")]} -- user2test
        req3 = req {requestHeaders = [("Authorization", "Basic bG9naW4zOk5PQ09SUkVDVFBBU1NXT1JE")]} -- user3test
        authHandle' = authHandle {Auth.validPassword = \_login _password -> pure $ Right False}
        handle' = handle {Handlers.Web.Base.auth = authHandle'}

    Handlers.Web.Base.client <$> evalState (doAuthorization handle' req1) usersInBase
      `shouldNotBe` Right (Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1test)) -- user1test
    Handlers.Web.Base.client <$> evalState (doAuthorization handle' req2) usersInBase
      `shouldNotBe` Right (Client (Just Proxy) (Just Proxy) (Just . MkLogin $ userLogin user2test)) -- user2test
    Handlers.Web.Base.client <$> evalState (doAuthorization handle' req3) usersInBase
      `shouldNotBe` Right (Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3test)) -- user3test

instance Show Response where
  show (ResponseBuilder s h b) = mconcat [show s, show h, show b]
  show _ = "Response"

instance Eq Response where
  (==) (ResponseBuilder s h b) (ResponseBuilder s' h' b') = (s == s') && (h == h') && (show b == show b')
  (==) _ _ = undefined
