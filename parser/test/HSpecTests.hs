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

        describe "combining multiple operations" $ do
            it "AND and NOT" $ do
                calculate "T AND NOT F" `shouldBe` True

