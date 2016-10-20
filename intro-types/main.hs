module IntroToTypes where

import Test.Hspec
import Text.Printf (printf)

type Serie a = [a]
type Cart a = [Serie a]

serie :: b -> Serie b
serie b = [b]

cart :: Serie s -> Cart s
cart s = [s]

main = hspec $ do
    describe "a simple type" $ do
        it "is equivalent to its primitive representation" $ do
            (serie 1) `shouldBe` [1];
            (serieFor [1,2,3]) `shouldBe` [1,2,3];
            (cart (serie 1)) `shouldBe` [[1]];

        it "can be manipulated as a list (underlying representation)" $ do
            (1:(serie 2)) `shouldBe` [1,2];
            
