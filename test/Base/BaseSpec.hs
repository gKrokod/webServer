module Base.BaseSpec (spec) where

import Test.Hspec
import Base.Base ()

spec :: Spec
spec = do
  describe "Base.Base" $ do
    it "removes leading and trailing whitespace" $ do
      (succ 2 :: Int) `shouldBe` (3 :: Int)
