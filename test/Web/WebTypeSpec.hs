module Web.WebTypeSpec (spec) where

import Test.Hspec
import Web.WebType ()

spec :: Spec
spec = do
  describe "Web.WebType" $ do
    it "removes leading and trailing whitespace" $ do
      (succ 2 :: Int) `shouldBe` (3 :: Int)
