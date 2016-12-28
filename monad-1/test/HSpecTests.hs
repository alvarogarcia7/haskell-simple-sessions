import Parser

import Test.Hspec
import Text.Printf (printf)

main = hspec $ do
  describe "canary" $ do
    it "truthy" $ do
      True `shouldBe` True

  describe "applyting operations" $ do
    it "And" $ do
      apply (And True True) `shouldBe` True
      apply (And True False) `shouldBe` False

    it "Not" $ do
      apply (Not True) `shouldBe` False
      apply (Not False) `shouldBe` True
