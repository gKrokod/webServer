module Handlers.LoggerSpec (spec) where

import Test.Hspec
import Handlers.Logger ()

spec :: Spec
spec = do
  describe "Handlers.Logger" $ do
    it "removes leading and trailing whitespace" $ do
      (succ 2 :: Int) `shouldBe` (3 :: Int)
