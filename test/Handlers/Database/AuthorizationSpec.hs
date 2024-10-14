module Handlers.Database.AuthorizationSpec (spec) where

import Handlers.Database.Authorization (getPrivilege)
import Test.Hspec
import Handlers.Database.Base (Handle (..))
import qualified Handlers.Logger
import Database.Data.FillTables (user1, user2, user3)
import Schema (User(..))
import Control.Monad.State (State,  evalState, gets)
import Types (Login(..))
import Data.Maybe (listToMaybe, mapMaybe)


spec :: Spec
spec = do
    let usersInBase = [user1, user2, user3]
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
                        ( \user@(User _ l _ _ _ _) ->
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
      evalState (getPrivilege baseHandle' (MkLogin $ userLogin user1)) usersInBase
        `shouldBe` Right (userIsAdmin user1, userIsPublisher user1)
      let baseHandle' = baseHandle
      evalState (getPrivilege baseHandle' (MkLogin $ userLogin user2)) usersInBase
        `shouldBe` Right (userIsAdmin user2, userIsPublisher user2)
      let baseHandle' = baseHandle
      evalState (getPrivilege baseHandle' (MkLogin $ userLogin user3)) usersInBase
        `shouldBe` Right (userIsAdmin user3, userIsPublisher user3)
