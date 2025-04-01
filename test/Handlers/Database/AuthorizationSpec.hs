module Handlers.Database.AuthorizationSpec (spec) where

import Control.Monad.State (State, evalState, gets)
import Data.Maybe (listToMaybe, mapMaybe)
import Database.Data.FillTables (user1test, user2test, user3test)
import qualified Handlers.Database.Auth
import Handlers.Database.Authorization (getPrivilege)
import qualified Handlers.Logger
import Schema (User (..))
import Test.Hspec
import Types (Login (..))

spec :: Spec
spec = do
  let usersInBase = [user1test, user2test, user3test]
      logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
            Handlers.Logger.writeLog = \_ -> pure ()
          }
      authHandle =
        Handlers.Database.Auth.Handle
          { Handlers.Database.Auth.logger = logHandle,
            Handlers.Database.Auth.validPassword = \_ _ -> pure $ Right False,
            Handlers.Database.Auth.client = Handlers.Database.Auth.Client Nothing Nothing Nothing,
            Handlers.Database.Auth.validCopyRight = \_ _ -> pure $ Right False,
            Handlers.Database.Auth.findUserByLogin =
              \(MkLogin login) ->
                gets
                  ( Right
                      . listToMaybe
                      . mapMaybe
                        ( \user@(User _ l _ _ _ _ _) ->
                            if l == login then Just user else Nothing
                        )
                  )
          } ::
          Handlers.Database.Auth.Handle (State [User])

  it "Get no privilege for a user that is not in the database" $ do
    evalState (getPrivilege authHandle (MkLogin "NoUser")) usersInBase
      `shouldBe` Right (False, False)

  it "Get privilege for a user that is in the database" $ do
    evalState (getPrivilege authHandle (MkLogin $ userLogin user1test)) usersInBase
      `shouldBe` Right (userIsAdmin user1test, userIsPublisher user1test)
    evalState (getPrivilege authHandle (MkLogin $ userLogin user2test)) usersInBase
      `shouldBe` Right (userIsAdmin user2test, userIsPublisher user2test)
    evalState (getPrivilege authHandle (MkLogin $ userLogin user3test)) usersInBase
      `shouldBe` Right (userIsAdmin user3test, userIsPublisher user3test)
