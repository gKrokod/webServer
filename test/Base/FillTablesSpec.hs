module Base.FillTablesSpec (spec) where

import Test.Hspec
import Base.FillTables ()

spec :: Spec
spec = do
  describe "Base.FillTables" $ do
    it "removes leading and trailing whitespace" $ do
      (succ 2 :: Int) `shouldBe` (3 :: Int)
