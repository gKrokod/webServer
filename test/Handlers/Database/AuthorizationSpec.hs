module Handlers.Database.AuthorizationSpec (spec) where

import Control.Monad.State (State, evalState, gets)
import Data.Maybe (listToMaybe, mapMaybe)
import Database.Data.FillTables (user1test, user2test, user3test)
import Handlers.Database.Authorization (getPrivilege)
import Handlers.Database.Base (Handle (..))
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

      baseHandle =
        Handle
          { logger = logHandle,
            findUserByLogin = \(MkLogin login) ->
              gets
                ( Right
                    . listToMaybe
                    . mapMaybe
                      ( \user@(User _ l _ _ _ _ _) ->
                          if l == login then Just user else Nothing
                      )
                )
          } ::
          Handle (State [User])
  it "Get no privilege for a user that is not in the database" $ do
    let baseHandle' = baseHandle
    evalState (getPrivilege baseHandle' (MkLogin "NoUser")) usersInBase
      `shouldBe` Right (False, False)

  it "Get privilege for a user that is in the database" $ do
    let baseHandle' = baseHandle
    evalState (getPrivilege baseHandle' (MkLogin $ userLogin user1test)) usersInBase
      `shouldBe` Right (userIsAdmin user1test, userIsPublisher user1test)
    let baseHandle' = baseHandle
    evalState (getPrivilege baseHandle' (MkLogin $ userLogin user2test)) usersInBase
      `shouldBe` Right (userIsAdmin user2test, userIsPublisher user2test)
    let baseHandle' = baseHandle
    evalState (getPrivilege baseHandle' (MkLogin $ userLogin user3test)) usersInBase
      `shouldBe` Right (userIsAdmin user3test, userIsPublisher user3test)
