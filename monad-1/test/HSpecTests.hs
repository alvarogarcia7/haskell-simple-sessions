import Parser

import Test.Hspec
import Text.Printf (printf)

true = Expression True
false = Expression False

main = hspec $ do
  describe "canary" $ do
    it "truthy" $ do
      True `shouldBe` True

  describe "applying operations" $ do
    it "And" $ do
      apply (And true true) `shouldBe` True
      apply (And true false) `shouldBe` False

    it "Not" $ do
      apply (Not true) `shouldBe` False
      apply (Not false) `shouldBe` True
  
  describe "nesting operations" $ do
    it "Not" $ do
      apply (Not (Not true)) `shouldBe` True

    it "Not with And" $ do
      apply (Not (And true true)) `shouldBe` False

