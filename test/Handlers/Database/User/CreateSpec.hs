{-# LANGUAGE TemplateHaskell #-}

module Handlers.Database.User.CreateSpec where

import Control.Monad.State (State, execState, modify)
import Database.Data.FillTables (user1test, user2test, user3test)
import Database.Data.LocalTime (localtimeTemplate)
import Handlers.Database.Base (Handle (..), Success (..))
import Handlers.Database.User.Create (createUserBase)
import qualified Handlers.Logger
import Handlers.Web.Base (UserInternal (..))
import Schema (User (..))
import Test.Hspec
import Types (Login (..), Name (..), PasswordUser (..))

spec :: Spec
spec = do
  let logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
            Handlers.Logger.writeLog = \_ -> pure ()
          }
      usersInBase = [user1test, user2test, user3test]
      baseHandle =
        Handle
          { logger = logHandle,
            findUserByLogin = undefined,
            getTime = pure (read $(localtimeTemplate)),
            putUser = \(UserInternal (MkName name) (MkLogin login) pass_ admin publish) time -> do
              modify (User name login undefined time admin publish undefined :)
              pure $ Right Put
          } ::
          Handle (State [User])
  it "Sucess: user does not exdfdfist in the database" $ do
    let baseHandle' = baseHandle {findUserByLogin = const (pure $ Right Nothing)}
    length (execState (createUserBase baseHandle' (UserInternal (MkName "Name") (MkLogin "Login") (MkPasswordUser "Password") False False)) usersInBase)
      `shouldBe` succ (length usersInBase)
  it "Failure: user exists in the database" $ do
    let baseHandle' = baseHandle {findUserByLogin = const (pure $ Right $ Just user1test)}
    length (execState (createUserBase baseHandle' (UserInternal (MkName "Name") (MkLogin "Login") (MkPasswordUser "Password") False False)) usersInBase)
      `shouldNotBe` succ (length usersInBase)

  it "Failure: error when working with database" $ do
    let baseHandle' = baseHandle {findUserByLogin = const (pure $ Left undefined)}
    length (execState (createUserBase baseHandle' (UserInternal (MkName "Name") (MkLogin "Login") (MkPasswordUser "Password") False False)) usersInBase)
      `shouldNotBe` succ (length usersInBase)
