module Handlers.Database.User.GetSpec where

import Control.Monad.Identity (Identity (..))
import Database.Data.FillTables (time4, user1test)
import Handlers.Database.Base (Limit (..), Offset (..), Success (..))
import Handlers.Database.User (Handle (..))
import Handlers.Database.User.Get (getAllUsers)
import qualified Handlers.Logger
import Schema (User (..))
import Test.Hspec
import Test.QuickCheck (property)

spec :: Spec
spec = do
  describe "Data should be limited (Paginate)" $ do
    let serverLimit = 15
        numberUserInBase = 27
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
              Handlers.Database.User.pullAllUsers =
                \(MkOffset userOffset_) (MkLimit userLimit_) ->
                  pure $
                    Right $
                      take (min userLimit_ serverLimit) $
                        drop userOffset_ $
                          map
                            (const (User "" "" undefined undefined undefined undefined undefined))
                            [1 .. numberUserInBase],
              Handlers.Database.User.findUserByLogin = \_ -> pure $ Right $ Just user1test,
              Handlers.Database.User.putUser = \_ _ -> pure $ Right Put
            } ::
            Handlers.Database.User.Handle Identity
    it "Random offset and limit" $ do
      property $ \offset limit -> do
        let offset' = max 0 offset
            limit' = max 0 limit
            baseUserHandle' = baseUserHandle {Handlers.Database.User.userOffset = offset', Handlers.Database.User.userLimit = limit'}
        length <$> runIdentity (getAllUsers baseUserHandle')
          `shouldBe` Right (minimum [max 0 (numberUserInBase - Handlers.Database.User.userOffset baseUserHandle'), serverLimit, Handlers.Database.User.userLimit baseUserHandle'])
