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

permutate :: [a] -> [[a]]
permutate [] = []