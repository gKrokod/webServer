{-# LANGUAGE TemplateHaskell #-}

module Handlers.Database.User.CreateSpec  where

import Test.Hspec
import Handlers.Database.User.Create (createUserBase)
import Handlers.Database.Base (Handle (..), Success(..))
import Database.Data.LocalTime (localtimeTemplate)
import qualified Handlers.Logger
import Database.Data.FillTables (user1, user2, user3)
import Handlers.Web.Base (UserInternal (..))
import Scheme (User(..))
import Control.Monad.State (State, execState, modify)
import Types (Name(..), Login(..),PasswordUser (..) )

spec :: Spec
spec = do
    let logHandle =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }
        usersInBase = [user1, user2, user3]
        baseHandle =
          Handle
            { logger = logHandle,
              findUserByLogin = undefined,
              getTime = pure (read $(localtimeTemplate)),
              putUser = \(UserInternal (MkName name) (MkLogin login) pass_ admin publish) time -> do
                modify (User name login undefined time admin publish :)
                pure $ Right Put
            } ::
            Handle (State [User])
    it "Sucess: user does not exdfdfist in the database" $ do
      let baseHandle' = baseHandle {findUserByLogin = const (pure $ Right Nothing)}
      length (execState (createUserBase baseHandle' (UserInternal (MkName "Name") (MkLogin "Login") (MkPasswordUser "Password") False False)) usersInBase)
        `shouldBe` succ (length usersInBase)
    it "Failure: user exists in the database" $ do
      let baseHandle' = baseHandle {findUserByLogin = const (pure $ Right $ Just user1)}
      length (execState (createUserBase baseHandle' (UserInternal (MkName "Name") (MkLogin "Login") (MkPasswordUser "Password") False False)) usersInBase)
        `shouldNotBe` succ (length usersInBase)

    it "Failure: error when working with database" $ do
      let baseHandle' = baseHandle {findUserByLogin = const (pure $ Left undefined)}
      length (execState (createUserBase baseHandle' (UserInternal (MkName "Name") (MkLogin "Login") (MkPasswordUser "Password") False False)) usersInBase)
        `shouldNotBe` succ (length usersInBase)
