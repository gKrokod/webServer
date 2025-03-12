module Handlers.Database.User.CreateSpec where

import Control.Monad.State (State, execState, modify)
import Database.Data.FillTables (time4, user1test, user2test, user3test)
import Handlers.Database.Base ( Success (..))
import Handlers.Database.User (Handle (..))
import Handlers.Database.User.Create (createUserBase)
import qualified Handlers.Logger
import Handlers.Web.Base (UserInternal (..))
import Schema (User (..))
import Test.Hspec
import Types (Login (..), Name (..), PasswordUser (..))

spec :: Spec
spec = do
  let usersInBase = [user1test, user2test, user3test]
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
            Handlers.Database.User.findUserByLogin = \_ -> pure $ Right $ Just user1test,
            Handlers.Database.User.putUser = 
              \(UserInternal (MkName name) (MkLogin login) _pass admin publish) time -> do
                modify (User name login undefined time admin publish undefined :)
                pure $ Right Put
          } :: Handlers.Database.User.Handle (State [User])

  it "Sucess: user does not exdfdfist in the database" $ do
    let baseUserHandle' = baseUserHandle {findUserByLogin = const (pure $ Right Nothing)}
    length (execState (createUserBase baseUserHandle' (UserInternal (MkName "Name") (MkLogin "Login") (MkPasswordUser "Password") False False)) usersInBase)
      `shouldBe` succ (length usersInBase)
  it "Failure: user exists in the database" $ do
    let baseUserHandle' = baseUserHandle {findUserByLogin = const (pure $ Right $ Just user1test)}
    length (execState (createUserBase baseUserHandle' (UserInternal (MkName "Name") (MkLogin "Login") (MkPasswordUser "Password") False False)) usersInBase)
      `shouldNotBe` succ (length usersInBase)

  it "Failure: error when working with database" $ do
    let baseUserHandle' = baseUserHandle {findUserByLogin = const (pure $ Left undefined)}
    length (execState (createUserBase baseUserHandle' (UserInternal (MkName "Name") (MkLogin "Login") (MkPasswordUser "Password") False False)) usersInBase)
      `shouldNotBe` succ (length usersInBase)
