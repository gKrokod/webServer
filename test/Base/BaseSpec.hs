module Base.BaseSpec (spec) where

import Test.Hspec
import Base.Base ()

spec :: Spec
spec = do
    it "nothing to test" $ do
      (succ 2 :: Int) `shouldBe` (3 :: Int)
