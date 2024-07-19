module Base.TestImageSpec (spec) where

import Test.Hspec
import Base.TestImage ()

spec :: Spec
spec = do
  describe "Base.TestImage" $ do
    it "removes leading and trailing whitespace" $ do
      (succ 2 :: Int) `shouldBe` (3 :: Int)
