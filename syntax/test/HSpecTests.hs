import Syntax

import Test.Hspec
import Text.Printf (printf)

main = hspec $ do
    describe "canary" $ do
      it "truthy" $ do
        True `shouldBe` True

    describe "let notation" $ do
      describe "let/in notation" $ do
        it "binds a value to the expression, making it available in that smaller scope" $ do
          letNotation `shouldBe` 1
      
      describe "let notation with a do block" $ do
        it "needs a do block to put several expressions together" $ do
          letNotation' `shouldBe` 2


