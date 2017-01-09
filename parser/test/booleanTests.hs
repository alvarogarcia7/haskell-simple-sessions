import ParserWithParsec

import Test.Hspec
import Text.Printf (printf)

main = hspec $ do
  describe "canary" $ do
    it "truthy" $ do
       True `shouldBe` True

  describe "parse expressions" $ do
    describe "simple expressions" $ do  
      it "parses T" $ do
        parseExpression "T" `shouldBe` (Right "T")

      it "parses F" $ do
        parseExpression "F" `shouldBe` (Right "F")

    describe "negated expressions" $ do  
      it "case 1" $ do
        parseExpression "NOT T" `shouldBe` (Right "F")
        parseExpression "NOT F" `shouldBe` (Right "T")

    describe "operation AND" $ do  
      it "case 1" $ do
        parseExpression "T AND T" `shouldBe` (Right "T")

      it "supports multiple expressions" $ do
        parseExpression "T AND T AND T" `shouldBe` (Right "T")
        parseExpression "T AND T AND F" `shouldBe` (Right "F")

    describe "operation OR" $ do  
      it "case 1" $ do
        parseExpression "F OR F" `shouldBe` (Right "F")

    describe "combining multiple operations" $ do
      it "AND and NOT" $ do
        parseExpression "T AND NOT F" `shouldBe` (Right "T")

--      it "NOT and NOT" $ do
--        parseExpression "NOT NOT F" `shouldBe` (Right "F")

