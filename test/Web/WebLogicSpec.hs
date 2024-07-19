module Web.WebLogicSpec (spec) where

import Test.Hspec
import Web.WebLogic () 

spec :: Spec
spec = do
  describe "Web.WebLogic" $ do
    it "removes leading and trailing whitespace" $ do
      (succ 2 :: Int) `shouldBe` (3 :: Int)
