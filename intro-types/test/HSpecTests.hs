import Test.Hspec
import IntroToTypes 
import Prelude hiding (flip)

main = hspec $ do
    describe "a simple type" $ do
        it "is equivalent to its primitive representation" $ do
            (serie 1) `shouldBe` [1];
            (serieFor [1,2,3]) `shouldBe` [1,2,3];
            (cart (serie 1)) `shouldBe` [[1]];

        it "can be manipulated as a list (underlying representation)" $ do
            (1:(serie 2)) `shouldBe` [1,2];


    describe "a cart" $ do
        it "has a length" $ do
            length' (cart [1]) `shouldBe` 1;
            length' ([2]:cart [1]) `shouldBe` 2;
            length' ([2,3]:cart [1]) `shouldBe` 3;
            length' ([3,4]:([2,3]:cart [1])) `shouldBe` 5;

    describe "flip cart" $ do
        it "flips an empty cart" $ do
           (flip (cart (serie 1))) `shouldBe` cart (serie 1);

