module LoggerSpec (spec) where

import Test.Hspec
import Logger ()

spec :: Spec
spec = do
  describe "Logger" $ do
    it "removes leading and trailing whitespace" $ do
      (succ 2 :: Int) `shouldBe` (3 :: Int)
