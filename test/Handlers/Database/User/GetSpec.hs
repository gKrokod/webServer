module Handlers.Database.User.GetSpec  where

import Handlers.Database.User.Get (getAllUsers)

import Test.Hspec
import Schema (User(..))
import Control.Monad.Identity (Identity(..))
import Test.QuickCheck (property)
import Handlers.Database.Base (Handle (..), Offset (..), Limit (..))


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
                          (const (User "" "" undefined undefined False False))
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

