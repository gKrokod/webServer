module ConfigSpec (spec) where

import Test.Hspec
import Config (helper)

spec :: Spec
spec = do
  describe "Config" $ do
    it "removes leading and trailing whitespace" $ do
      (helper 2 :: Int) `shouldBe` (3 :: Int)
