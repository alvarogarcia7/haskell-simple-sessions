import Permutations

import Test.Hspec
import Text.Printf (printf)

main = hspec $ do
    describe "generates permutations" $ do
        it "for the empty list" $ do
            (permutations [] :: [[Int]]) `shouldBe` ([[]] :: [[Int]])

        it "for a one-element list" $ do
            (permutations [1] :: [[Int]]) `shouldBe` ([[1]] :: [[Int]])

        it "for a multiple-element list" $ do
            (permutations [1,2] :: [[Int]]) `shouldBe` ([[1,2], [2,1]] :: [[Int]]);
        
        it "for a multiple-element list" $ do
            (length (permutations [1,2,3] :: [[Int]])) `shouldBe` (6::Int);
            (permutations [1,2,3] :: [[Int]]) `shouldBe` ([[1,2,3],[1,3,2],[3,1,2],[2,1,3],[2,3,1],[3,2,1]]::[[Int]]);


