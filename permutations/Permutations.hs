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

permutate :: [a] -> [[a]]
permutate [] = []