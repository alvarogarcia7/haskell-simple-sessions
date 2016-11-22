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

