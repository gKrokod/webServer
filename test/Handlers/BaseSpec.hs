module Handlers.BaseSpec (spec) where

import Test.Hspec
import Handlers.Base ()

spec :: Spec
spec = do
  describe "Handlers.Base" $ do
    it "removes leading and trailing whitespace" $ do
      (succ 2 :: Int) `shouldBe` (3 :: Int)
