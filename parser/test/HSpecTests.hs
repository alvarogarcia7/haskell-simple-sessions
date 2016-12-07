import Parser

import Test.Hspec
import Text.Printf (printf)

main = hspec $ do
  describe "canary" $ do
    it "truthy" $ do
       True `shouldBe` True

  describe "parse expressions" $ do
    describe "simple expressions" $ do  
      it "parses T" $ do
        calculate "T" `shouldBe` True

      it "parses F" $ do
        calculate "F" `shouldBe` False

    describe "negated expressions" $ do  
      it "case 1" $ do
        calculate "NOT T" `shouldBe` False
        calculate "NOT F" `shouldBe` True

    describe "operation AND" $ do  
      it "case 1" $ do
        calculate "T AND T" `shouldBe` True

      it "supports multiple expressions" $ do
        calculate "T AND T AND T" `shouldBe` True
        calculate "T AND T AND F" `shouldBe` False

    describe "operation OR" $ do  
      it "case 1" $ do
        calculate "F OR F" `shouldBe` False

    describe "combining multiple operations" $ do
      it "AND and NOT" $ do
        calculate "T AND NOT F" `shouldBe` True

      it "NOT and NOT" $ do
        calculate "NOT NOT F" `shouldBe` False

  describe "new method" $ do
    it "evaluates an expression of one operator" $ do
      (apply $ parse "T AND T") `shouldBe` True
      (apply $ parse "T AND F") `shouldBe` False
      (apply $ parse "F AND F") `shouldBe` False
      (apply $ parse "F AND T") `shouldBe` False

    it "evaluates an expression of the operator OR" $ do
      (apply $ parse "T OR T") `shouldBe` True
      (apply $ parse "T OR F") `shouldBe` True
      (apply $ parse "F OR F") `shouldBe` False
      (apply $ parse "F OR T") `shouldBe` True

    it "evaluates an expression with multiple AND operators" $ do
      (apply $ parse "T AND T AND F") `shouldBe` False
      (apply $ parse "T AND T AND T AND F") `shouldBe` False
      (apply $ parse "T AND T AND T AND F AND T AND T AND T AND T") `shouldBe` False

    it "evaluates an expression with multiple OR operators" $ do
      (apply $ parse "T OR T OR F") `shouldBe` True
      (apply $ parse "T OR T OR T OR F") `shouldBe` True
      (apply $ parse "T OR T OR T OR F OR T OR T OR T OR T") `shouldBe` True

    it "evaluates a mix of OR and AND" $ do
      -- the order of the parens does not matter
      (apply $ parse "T OR F AND T") `shouldBe` True 
      (apply $ parse "T OR F AND T AND F") `shouldBe` False 

    it "evaluates NOT" $ do
      (apply $ parse "NOT F") `shouldBe` True 
      (apply $ parse "NOT NOT F") `shouldBe` False
      (apply $ parse "NOT NOT NOT NOT F") `shouldBe` False

