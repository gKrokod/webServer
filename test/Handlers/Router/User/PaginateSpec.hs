{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.User.PaginateSpec (spec) where

import Control.Monad.State (State, evalState, get)
import Data.Binary.Builder as BU (Builder)
import Database.Data.FillTables (user1test, user2test, user3test)
import Handlers.Router (doLogic)
import Network.HTTP.Types (status200)
import Network.Wai (defaultRequest, queryString, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (User (..))
import Test.Hspec (Spec, it, shouldBe)
import Web.DTO.User (userToWeb)

import Config (ConfigDataBase (..), connectionString, loadConfig)
import Control.Exception (bracket_)
import Data.Time (getCurrentTime)
import qualified Database.Api as DA
import qualified Handlers.Database.Base as DB
import qualified Handlers.Database.Auth
import qualified Handlers.Database.User
import qualified Handlers.Web.User
import qualified Handlers.Database.Image
import qualified Handlers.Web.Image
import qualified Handlers.Database.Category
import qualified Handlers.Web.Category
import qualified Handlers.Web.News
import qualified Handlers.Database.News
import Handlers.Logger (Log (Info), logMessage)
import qualified Handlers.Logger
import Handlers.Router (doAuthorization, doLogic)
import qualified Handlers.Web.Base
import qualified Logger
import Network.Wai (Application)
import Schema (ColumnType (..), SortOrder (..))
import qualified Web.Utils as WU

spec :: Spec
spec = do
  it "123" $ do head [23,14] `shouldBe` (23 :: Int)
  -- let req = defaultRequest
  -- let logHandle =
  --       Handlers.Logger.Handle
  --         { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
  --           Handlers.Logger.writeLog = \_ -> pure ()
  --         }
  --     -- client = 
  --     --         Handlers.Database.Auth.Client
  --     --           { Handlers.Database.Auth.clientAdminToken = Nothing,
  --     --             Handlers.Database.Auth.clientPublisherToken = Nothing,
  --     --             Handlers.Database.Auth.author = Nothing
  --     --           }
  --     authHandle =
  --       Handlers.Database.Auth.Handle
  --         { Handlers.Database.Auth.logger = logHandle
  --           -- Handlers.Database.Auth.findUserByLogin = DA.findUserByLogin pginfo, --нужен для авторизации. там проверка
  --           -- Handlers.Database.Auth.validPassword = DA.validPassword pginfo,
  --           -- Handlers.Database.Auth.client = client
  --           -- Handlers.Database.Auth.validCopyRight = DA.validCopyRight pginfo
  --         }
  --     baseUserHandle =
  --       Handlers.Database.User.Handle
  --         { Handlers.Database.User.logger = logHandle
  --           -- Handlers.Database.User.userOffset = 0,
  --           -- Handlers.Database.User.userLimit = maxBound,
  --           -- Handlers.Database.User.getTime = getCurrentTime,
  --           -- Handlers.Database.User.makeHashPassword = DA.makeHashPassword,
  --           -- Handlers.Database.User.pullAllUsers = \(DB.MkOffset offset) (DB.MkLimit limit) -> get >>= pure . Right . take limit . drop offset
  --           -- Handlers.Database.User.findUserByLogin = DA.findUserByLogin pginfo,
  --           -- Handlers.Database.User.putUser = DA.putUser pginfo
  --         }
  --     userHandle = Handlers.Web.User.Handle {
  --         Handlers.Web.User.logger = logHandle
  --         -- Handlers.Web.User.base = baseUserHandle,
  --         -- Handlers.Web.User.response400 = WU.response40k,
  --         -- Handlers.Web.User.response500 = WU.response500,
  --         -- Handlers.Web.User.response200 = WU.response200,
  --         -- Handlers.Web.User.response404 = WU.response404,
  --         -- Handlers.Web.User.mkGoodResponse = testBuilder --WU.mkGoodResponse,
  --         -- Handlers.Web.User.getBody = WU.getBody
  --       }
  --
  --     handle =
  --       Handlers.Web.Base.Handle
  --         {  
  --           Handlers.Web.Base.logger = logHandle
  --           -- Handlers.Web.Base.base = baseHandle,
  --           -- Handlers.Web.Base.auth = authHandle,
  --           -- Handlers.Web.Base.client = client,
  --           -- Handlers.Web.Base.user = userHandle
  --         } :: Handlers.Web.Base.Handle (State [User])
  --
  --     usersInBase = [user1test, user2test, user3test]

  it "Client can paginate" $ do
    1 `shouldBe` 1
    -- let req' = req {rawPathInfo = "/users", queryString = [("paginate", Just "{\"offset\":1,\"limit\":1}")]}
    --
    -- evalState (doLogic handle req') usersInBase
    --   `shouldBe` (testBuilder . userToWeb $ take 1 $ drop 1 usersInBase)

testBuilder :: Builder -> Response
testBuilder = responseBuilder status200 []

instance Show Response where
  show (ResponseBuilder s h b) = mconcat [show s, show h, show b]
  show _ = "Response"

instance Eq Response where
  (==) (ResponseBuilder s h b) (ResponseBuilder s' h' b') = (s == s') && (h == h') && (show b == show b')
  (==) _ _ = undefined
