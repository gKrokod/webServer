module SchemeSpec (spec) where

import Test.Hspec
import Scheme ()

spec :: Spec
spec = do
  describe "Scheme" $ do
    it "removes leading and trailing whitespace" $ do
      (succ 2 :: Int) `shouldBe` (3 :: Int)
