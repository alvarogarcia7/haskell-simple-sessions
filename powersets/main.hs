module PowerSets where

import Test.Hspec
import Data.List
import Text.Printf (printf)

powersets :: [x] -> [[x]]
powersets xs = sortBy size $ p' xs

size a b | length a > length b = GT
              | length a < length b = LT
              | length a == length b = EQ


p' :: [x] -> [[x]]
p' [] = [[]]
p' xs = flatmap f ( tails xs) where
    f [] = [[]]
    f (x':xs') = map (\xs'' -> x':xs'') (p' xs')

flatmap :: (x -> [x]) -> [x] -> [x]
flatmap f xs = foldl (\acc ele-> acc ++ (f ele)) [] xs

main = hspec $ do
        describe "canary" $ do
            it "truth" $ do
                True `shouldBe` True;

        describe "powersets" $ do
            it "base case" $ do
                ((powersets []) :: [[Int]]) `shouldBe` [[]];

            it "input of one" $ do
                ((powersets [1]) :: [[Int]]) `shouldBe` [[],[1]];

            it "input of two" $ do
                ((powersets [1,2]) :: [[Int]]) `shouldBe` [[],[1],[2],[1,2]];

            it "input of three" $ do
                ((powersets [1,2,3]) :: [[Int]]) `shouldBe` [[],[1],[2],[3],[1,2],[1,3],[2,3],[1,2,3]];


