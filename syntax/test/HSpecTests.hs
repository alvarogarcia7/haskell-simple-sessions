import Syntax

import Test.Hspec
import Text.Printf (printf)

main = hspec $ do
    describe "canary" $ do
        it "truthy" $ do
           True `shouldBe` True
