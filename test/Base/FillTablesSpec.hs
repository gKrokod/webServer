module Base.FillTablesSpec (spec) where

import Base.FillTables ()
import Test.Hspec

spec :: Spec
spec = do
  it "nothing to test" $ do
    (succ 2 :: Int) `shouldBe` (3 :: Int)
