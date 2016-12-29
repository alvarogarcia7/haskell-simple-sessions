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
    it "AND" $ do
      apply (Op2 "AND" true true) `shouldBe` True
      apply (Op2 "AND" true false) `shouldBe` False

    it "NOT" $ do
      apply (Op1 "NOT" true) `shouldBe` False
      apply (Op1 "NOT" false) `shouldBe` True
  
  describe "nesting operations" $ do
    it "NOT" $ do
      apply (Op1 "NOT" (Op1 "NOT" true)) `shouldBe` True

    it "NOT with AND" $ do
      apply (Op1 "NOT" (Op2 "AND" true true)) `shouldBe` False

