import Parser

import Test.Hspec
import Text.Printf (printf)

true = Literal True
false = Literal False

main = hspec $ do
  describe "canary" $ do
    it "truthy" $ do
      True `shouldBe` True

  describe "applying operations" $ do
    it "Op2" $ do
      apply (Op2 true true) `shouldBe` True
      apply (Op2 true false) `shouldBe` False

    it "Op1" $ do
      apply (Op1 true) `shouldBe` False
      apply (Op1 false) `shouldBe` True
  
  describe "nesting operations" $ do
    it "Op1" $ do
      apply (Op1 (Op1 true)) `shouldBe` True

    it "Op1 with Op2" $ do
      apply (Op1 (Op2 true true)) `shouldBe` False

