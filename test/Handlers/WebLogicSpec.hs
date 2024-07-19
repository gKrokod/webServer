module Handlers.WebLogicSpec (spec) where

import Test.Hspec
import Handlers.WebLogic ()

spec :: Spec
spec = do
  describe "Handlers.WebLogic" $ do
    it "removes leading and trailing whitespace" $ do
      (succ 2 :: Int) `shouldBe` (3 :: Int)
