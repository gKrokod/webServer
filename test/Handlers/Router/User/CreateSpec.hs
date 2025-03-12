{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.User.CreateSpec (spec) where

import Control.Monad.State (State, evalState, gets)
import Data.Binary.Builder as BU (fromByteString)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Database.Data.FillTables (time4, user1test, user2test, user3test)
import qualified Handlers.Database.Base as DB
import qualified Handlers.Logger
import Handlers.Router (doLogic)
import Handlers.Web.Base (UserInternal (..))
import qualified Handlers.Web.Base as WB
import Network.HTTP.Types (badRequest400, forbidden403, internalServerError500, notFound404, status200)
import Network.Wai (defaultRequest, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (User (..))
import Test.Hspec (Spec, it, shouldBe, shouldNotBe)
import Types (Login (..))
import Handlers.Database.Auth (Client (..))
import qualified Handlers.Database.Auth as Auth

spec :: Spec
spec = do
  it "123" $ do head [23,14] `shouldBe` (23 :: Int)
  -- curl "127.0.0.1:4221/images?id=1" --output -
--   let req = defaultRequest
--       req' = req {rawPathInfo = "/users/create"}
--       logHandle =
--         Handlers.Logger.Handle
--           { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
--             Handlers.Logger.writeLog = \_ -> pure ()
--           }
--
--       usersInBase = [user1test, user2test, user3test]
--       baseHandle =
--         DB.Handle
--           { DB.logger = logHandle,
--             DB.getTime = pure time4,
--             DB.findUserByLogin = \(MkLogin login) ->
--               gets
--                 ( Right
--                     . listToMaybe
--                     . mapMaybe
--                       ( \user@(User _ l _ _ _ _ _) ->
--                           if l == login then Just user else Nothing
--                       )
--                 ),
--             DB.putUser = \(UserInternal _name _login _pass _admin _publish) _time -> pure $ Right DB.Put
--           }
--       webHandle =
--         WB.Handle
--           { WB.logger = logHandle,
--             WB.base = baseHandle,
--             WB.user = undefined
--             -- WB.response404 = test404,
--             -- WB.response403 = test403,
--             -- WB.response400 = test400,
--             -- WB.response500 = test500,
--             -- WB.response200 = test200
--           } ::
--           WB.Handle (State [User])
--
-- -- data Handle m = Handle
-- --   { 
-- --     logger :: Handlers.Logger.Handle m,
-- --     base :: Handlers.Database.User.Handle m,
-- --     getBody :: Request -> m B.ByteString,
-- --     response200 :: Response,
-- --     response400 :: Text -> Response,
-- --     response500 :: Response,
-- --     response404 :: Response,
-- --     mkGoodResponse :: Builder -> Response
-- --   }
-- -- import Handlers.Web.User (Handle(..))
-- -- data Handle m = Handle
-- --   { logger :: Handlers.Logger.Handle m,
-- --     validPassword :: Login -> PasswordUser -> m (Either SomeException Bool),
-- --     validCopyRight :: Login -> Title -> m (Either SomeException Bool),
-- --     client :: Client,
-- --     findUserByLogin :: Login -> m (Either SomeException (Maybe User))
-- --   }
--
-- -- data Handle m = Handle
-- --   { connectionString :: ConnectionString, 
-- --     logger :: Handlers.Logger.Handle m,
-- --     base :: Handlers.Database.Base.Handle m,
-- --     auth :: Handlers.Database.Auth.Handle m,
-- --     user :: Handlers.Web.User.Handle m,
-- --     category :: Handlers.Web.Category.Handle m,
-- --     image :: Handlers.Web.Image.Handle m,
-- --     news :: Handlers.Web.News.Handle m,
-- --     client :: Handlers.Database.Auth.Client
-- --   }
-- --
--   it "Admin can create new user" $ do
--     let bodyReq = "{\"isAdmin\":true,\"isPublisher\":true,\"login\":\"Dager\",\"name\":\"Petr\",\"password\":\"qwerty\"}"
--         baseHandle' = baseHandle
--         clientAdminUser1 = WB.Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1test)
--         webHandle' =
--           webHandle
--             { WB.base = baseHandle',
--               WB.client = clientAdminUser1,
--               WB.getBody = const . pure $ bodyReq
--             }
--
--     evalState (doLogic webHandle' req') usersInBase
--       `shouldBe` test200
--
--   it "Non-admin can't create a new user" $ do
--     let bodyReq = "{\"isAdmin\":true,\"isPublisher\":true,\"login\":\"Dager\",\"name\":\"Petr\",\"password\":\"qwerty\"}"
--         baseHandle' = baseHandle
--         clientAdminUser1 = WB.Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3test)
--         webHandle' =
--           webHandle
--             { WB.base = baseHandle',
--               WB.client = clientAdminUser1,
--               WB.getBody = const . pure $ bodyReq
--             }
--
--     evalState (doLogic webHandle' req') usersInBase
--       `shouldNotBe` test200
--
--   it "Admin can't create a new user with login that already exists in the databse" $ do
--     let clientAdminUser1 = WB.Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1test)
--         oldUser = E.encodeUtf8 . userLogin $ user1test
--         bodyReq = "{\"isAdmin\":true,\"isPublisher\":true,\"login\":\"" <> oldUser <> "\",\"name\":\"\",\"password\":\"qwerty\"}"
--         baseHandle' = baseHandle
--         webHandle' =
--           webHandle
--             { WB.base = baseHandle',
--               WB.client = clientAdminUser1,
--               WB.getBody = const . pure $ bodyReq
--             }
--
--     evalState (doLogic webHandle' req') usersInBase
--       `shouldNotBe` test200

test200 :: Response
test200 = responseBuilder status200 [] "All ok. status 200\n"

test400 :: T.Text -> Response
test400 = responseBuilder badRequest400 [] . fromByteString . E.encodeUtf8

test403 :: Response
test403 = responseBuilder forbidden403 [] "Forbidden. status 403\n"

test404 :: Response
test404 = responseBuilder notFound404 [] "NotFound. status 404\n"

test500 :: Response
test500 = responseBuilder internalServerError500 [] "internalServerError. status 500\n"

instance Show Response where
  show (ResponseBuilder s h b) = mconcat [show s, show h, show b]
  show _ = "Response"

instance Eq Response where
  (==) (ResponseBuilder s h b) (ResponseBuilder s' h' b') = (s == s') && (h == h') && (show b == show b')
  (==) _ _ = undefined
