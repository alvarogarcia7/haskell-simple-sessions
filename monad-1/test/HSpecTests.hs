import Parser

import Test.Hspec
import Text.Printf (printf)

main = hspec $ do
  describe "canary" $ do
    it "truthy" $ do
      True `shouldBe` True

  describe "applyting operations" $ do
    it "And" $ do
      apply (And (Expression True) (Expression True)) `shouldBe` True
      apply (And (Expression True) (Expression False)) `shouldBe` False

    it "Not" $ do
      apply (Not (Expression True)) `shouldBe` False
      apply (Not (Expression False)) `shouldBe` True
