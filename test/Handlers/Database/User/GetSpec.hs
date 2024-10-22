module Handlers.Database.User.GetSpec where

import Control.Monad.Identity (Identity (..))
import Handlers.Database.Base (Handle (..), Limit (..), Offset (..))
import Handlers.Database.User.Get (getAllUsers)
import Schema (User (..))
-- import qualified Database.Migrations.Migrationv0 as SOLD
-- import Schema (User (..))
import Test.Hspec
import Test.QuickCheck (property)

spec :: Spec
spec = do
  describe "Data should be limited (Panigate)" $ do
    let serverLimit = 15
        numberUserInBase = 27
        baseHandle =
          Handle
            { pullAllUsers = \(MkOffset userOffset_) (MkLimit userLimit_) ->
                pure $
                  Right $
                    take (min userLimit_ serverLimit) $
                      drop userOffset_ $
                        map
                          (const (User "" "" undefined undefined undefined undefined undefined))
                          -- (const (User "" "" undefined undefined undefined undefined undefined False False ))
                          [1 .. numberUserInBase]
            } ::
            Handle Identity
    it "Random offset and limit" $ do
      property $ \offset limit -> do
        let offset' = max 0 offset
            limit' = max 0 limit
            baseHandle' = baseHandle {userOffset = offset', userLimit = limit'}
        length <$> runIdentity (getAllUsers baseHandle')
          `shouldBe` Right (minimum [max 0 (numberUserInBase - userOffset baseHandle'), serverLimit, userLimit baseHandle'])
