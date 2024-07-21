module Web.WebTypeSpec (spec) where

import Test.Hspec
import Web.WebType ()

spec :: Spec
spec = do
    it "nothing to test" $ do
      (succ 2 :: Int) `shouldBe` (3 :: Int)
