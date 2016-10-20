module IntroToTypes where

import Test.Hspec
import Text.Printf (printf)
import Prelude hiding (flip)

type Serie a = [a]
type Cart a = [Serie a]

serie :: b -> Serie b
serie b = [b]

serieFor :: [b] -> Serie b
serieFor = id

cart :: Serie s -> Cart s
cart s = [s]

length' :: Cart c -> Int
length' [serie] = length'' serie
length' (serie:restOfCart) = length'' serie + length' restOfCart

length'' :: Serie s -> Int
length'' [] = 0
length'' (book:restOfSerie) = 1 + length'' restOfSerie

flip :: Cart c -> Cart c
flip = id

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

