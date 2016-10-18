import Test.Hspec
import Test.QuickCheck
import Data.List

main = hspec $ do
    describe "Canary Test" $ do
      it "should be green" $ do
          True `shouldBe` True

    describe "permutation" $ do
      it "of an empty array should be empty" $ do
        null (permutate []) `shouldBe` True

      it "of a 1-element array should be unique" $ do
        (permutate [1]) `shouldBe` ([[1]] :: [[Int]])

      describe "of a 2-element array" $ do
      	it "no repeated elements" $ do
          (permutate [1,2]) `shouldBe` ([[1,2], [2,1]] :: [[Int]])

        it "with repeated elements" $ do
          (permutate [3,3]) `shouldBe` ([[3,3], [3,3]] :: [[Int]]) --because permutations are performed on sets, therefore the two threes (3) are different


      describe "of a 3-element array" $ do
      	it "no repeated elements" $ do
          (permutate [1,2,3]) `shouldBe` ([[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1]] :: [[Int]])



permutate :: [a] -> [[a]]
permutate [] = []
permutate x = case length x of
	1 -> [x]
	_ -> x : [reverse x]