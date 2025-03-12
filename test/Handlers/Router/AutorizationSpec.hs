{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.AutorizationSpec (spec) where

import Control.Monad.State (State, evalState, gets)
-- import Data.Binary.Builder as BU (fromByteString)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
-- import qualified Data.Text as T
-- import qualified Data.Text.Encoding as E
import Database.Data.FillTables (user1test, user2test, user3test)
-- import qualified Handlers.Database.Base as DB
import qualified Handlers.Database.Auth as Auth
import qualified Handlers.Logger
import Handlers.Router (doAuthorization)
import qualified Handlers.Web.Base as WB
-- import Network.HTTP.Types (badRequest400, forbidden403, internalServerError500, notFound404)
import Network.Wai (defaultRequest, requestHeaders)
-- import Network.Wai (defaultRequest, requestHeaders, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (User (..))
import Test.Hspec (Spec, it, shouldBe, shouldNotBe)
import Types (Login (..))
import Handlers.Database.Auth (Client (..))

spec :: Spec
spec = do
  -- user1test admin noPublisher
  -- user2test admin publisher
  -- user3test noAdmin publisher
  -- unknown user noAdmin noPublisher
  let req = defaultRequest 

      logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
            Handlers.Logger.writeLog = \_ -> pure ()
          }

      usersInBase = [user1test, user2test, user3test]
      authHandle =
        Auth.Handle
          { Auth.logger = logHandle,
            Auth.findUserByLogin = \(MkLogin login) ->
              gets
                ( Right
                    . listToMaybe
                    . mapMaybe
                      ( \user@(User _ l _ _ _ _ _) ->
                          if l == login then Just user else Nothing
                      )
                ),
            Auth.validPassword = \_login _password -> pure $ Right True,
            Auth.client = undefined,
            Auth.validCopyRight = undefined 
          }
      webHandle =
        WB.Handle
          { WB.logger = logHandle,
            WB.auth = authHandle
          } ::
          WB.Handle (State [User])

--
-- data Handle m = Handle
--   { logger :: Handlers.Logger.Handle m,
--     validPassword :: Login -> PasswordUser -> m (Either SomeException Bool),
--     validCopyRight :: Login -> Title -> m (Either SomeException Bool),
--     client :: Client,
--     findUserByLogin :: Login -> m (Either SomeException (Maybe User))
--   }

-- data Handle m = Handle
--   { connectionString :: ConnectionString, 
--     logger :: Handlers.Logger.Handle m,
--     base :: Handlers.Database.Base.Handle m,
--     auth :: Handlers.Database.Auth.Handle m,
--     user :: Handlers.Web.User.Handle m,
--     category :: Handlers.Web.Category.Handle m,
--     image :: Handlers.Web.Image.Handle m,
--     news :: Handlers.Web.News.Handle m,
--     client :: Handlers.Database.Auth.Client
--   }

  it "Unknown user does not received privileges" $ do
    let req' = req {requestHeaders = []}
        webHandle' = webHandle

    WB.client <$> evalState (doAuthorization webHandle' req') usersInBase
      `shouldBe` Right (Client Nothing Nothing Nothing)

  it "A user with a valid password receives their privileges" $ do
    let req1 = req {requestHeaders = [("Authorization", "Basic bG9naW4xOnFwYXNzMQ==")]} -- user1test
        req2 = req {requestHeaders = [("Authorization", "Basic bG9naW4yOnFwYXNzMg==")]} -- user2test
        req3 = req {requestHeaders = [("Authorization", "Basic bG9naW4zOnFwYXNzMw==")]} -- user3test
        webHandle' = webHandle

    WB.client <$> evalState (doAuthorization webHandle' req1) usersInBase
      `shouldBe` Right (Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1test)) -- user1test
    WB.client <$> evalState (doAuthorization webHandle' req2) usersInBase
      `shouldBe` Right (Client (Just Proxy) (Just Proxy) (Just . MkLogin $ userLogin user2test)) -- user2test
    WB.client <$> evalState (doAuthorization webHandle' req3) usersInBase
      `shouldBe` Right (Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3test)) -- user3test
  it "A user with an invalid password does not receive his privileges" $ do
    let req1 = req {requestHeaders = [("Authorization", "Basic bG9naW4xOk5PQ09SUkVDVFBBU1NXT1JE")]} -- user1test
        req2 = req {requestHeaders = [("Authorization", "Basic bG9naW4yOk5PQ09SUkVDVFBBU1NXT1JE")]} -- user2test
        req3 = req {requestHeaders = [("Authorization", "Basic bG9naW4zOk5PQ09SUkVDVFBBU1NXT1JE")]} -- user3test
        authHandle' = authHandle {Auth.validPassword = \_login _password -> pure $ Right False}
        webHandle' = webHandle {WB.auth = authHandle'}

    WB.client <$> evalState (doAuthorization webHandle' req1) usersInBase
      `shouldNotBe` Right (Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1test)) -- user1test
    WB.client <$> evalState (doAuthorization webHandle' req2) usersInBase
      `shouldNotBe` Right (Client (Just Proxy) (Just Proxy) (Just . MkLogin $ userLogin user2test)) -- user2test
    WB.client <$> evalState (doAuthorization webHandle' req3) usersInBase
      `shouldNotBe` Right (Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3test)) -- user3test

-- test400 :: T.Text -> Response
-- test400 = responseBuilder badRequest400 [] . fromByteString . E.encodeUtf8
--
-- test403 :: Response
-- test403 = responseBuilder forbidden403 [] "Forbidden. status 403\n"
--
-- test404 :: Response
-- test404 = responseBuilder notFound404 [] "NotFound. status 404\n"
--
-- test500 :: Response
-- test500 = responseBuilder internalServerError500 [] "internalServerError. status 500\n"

instance Show Response where
  show (ResponseBuilder s h b) = mconcat [show s, show h, show b]
  show _ = "Response"

instance Eq Response where
  (==) (ResponseBuilder s h b) (ResponseBuilder s' h' b') = (s == s') && (h == h') && (show b == show b')
  (==) _ _ = undefined
