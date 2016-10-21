module Permutations where

import Test.Hspec
import Text.Printf (printf)

main = hspec $ do
    describe "generates permutations" $ do
        it "for the empty list" $
            (permutations [] :: [[Int]]) `shouldBe` ([[]] :: [[Int]])

        it "for a one-element list" $
            (permutations [1] :: [[Int]]) `shouldBe` ([[1]] :: [[Int]])

        it "for a multiple-element list" $
            (permutations [1,2] :: [[Int]]) `shouldBe` ([[1,2], [2,1]] :: [[Int]]);
            length (permutations [1,2,3] :: [[Int]]) `shouldBe` 6;
