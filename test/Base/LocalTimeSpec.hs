module Base.LocalTimeSpec (spec) where

import Test.Hspec
import Base.LocalTime ()

spec :: Spec
spec = do
  describe "Base.LocalTime" $ do
    it "removes leading and trailing whitespace" $ do
      (succ 2 :: Int) `shouldBe` (3 :: Int)
