module Web.WebLogicSpec (spec) where

import Test.Hspec
import Web.WebLogic ()

spec :: Spec
spec = do
  it "nothing to test" $ do
    (succ 2 :: Int) `shouldBe` (3 :: Int)
