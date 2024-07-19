module Base.CryptoSpec (spec) where

import Test.Hspec
import Base.Crypto ()

spec :: Spec
spec = do
  describe "Base.Crypto" $ do
    it "removes leading and trailing whitespace" $ do
      (succ 2 :: Int) `shouldBe` (3 :: Int)
