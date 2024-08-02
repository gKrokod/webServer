module Base.LocalTimeSpec (spec) where

import Base.LocalTime ()
import Test.Hspec

spec :: Spec
spec = do
  it "nothing to test" $ do
    (succ 2 :: Int) `shouldBe` (3 :: Int)
